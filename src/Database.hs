{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database(redoMetaDir, initializeMetaDepsDir, isSourceFile, storeIfChangeDependencies, storeIfCreateDependencies, 
                storeAlwaysDependency, upToDate, storePhonyTarget, createLockFile, removeLockFiles, markTargetClean, 
                markTargetDirty, markTargetBuilt, getTargetTimeStamp, metaFileDir, getTargetBuiltTimeStamp, findDoFile,
                initializeMetaDepsDir', safeGetTargetTimeStamp, whenTargetNotModified, Stamp, Target(..), DoFile(..),
                MetaDir(..), LockFile(..), doesTargetExist, performActionInDir) where

import Control.Applicative ((<$>),(<*>))
import Control.Exception (catchJust, catch, SomeException(..))
import Control.Monad (guard, liftM, filterM)
import qualified Data.ByteString.Char8 as BS
import Crypto.Hash.MD5 (hash) 
import Data.Hex (hex)
import Data.Bool (bool)
import Data.Maybe (isNothing, fromJust, listToMaybe)
import System.Directory (removeDirectoryRecursive, setCurrentDirectory, getAppUserDataDirectory, doesFileExist, getDirectoryContents, createDirectoryIfMissing, getCurrentDirectory, doesDirectoryExist)
import System.FilePath (takeExtensions, takeExtension, dropExtension, dropExtensions, isDrive, normalise, dropTrailingPathSeparator, makeRelative, splitFileName, (</>), takeDirectory, isPathSeparator, pathSeparator, takeExtension)
import System.Environment (getEnv)
import System.Exit (exitFailure)
import System.Posix.Files (getFileStatus, modificationTimeHiRes, fileID, fileSize)
import System.IO.Error (isDoesNotExistError)

import PrettyPrint
import Helpers

-- Type Definitions:
newtype Stamp = Stamp { unStamp :: BS.ByteString } deriving (Show, Eq) -- Timestamp or hash stamp of a file
newtype DoFile = DoFile { unDoFile :: FilePath } deriving (Show, Eq) -- The absolute path to a do file
newtype Target = Target { unTarget :: FilePath } deriving (Eq) -- The absolute path to a target file
newtype MetaDir = MetaDir { unMetaDir :: FilePath } deriving (Eq) -- The meta directory associated with a target
newtype MetaFile = MetaFile { unMetaFile :: FilePath } deriving (Eq) -- A meta file stored within a meta directory
newtype LockFile = LockFile { lockFileToFilePath :: FilePath } deriving (Eq) -- A lock file for synchronizing access to meta directories
newtype DatabaseEntry target dofile depdir = DatabaseEntry (Target, DoFile, MetaDir)

-- Some #defines used for creating escaped dependency filenames. We want to avoid /'s.
#define seperator_replacement '^'
#define seperator_replacement_escape '@'
-- We use different file prepends to denote different kinds of dependencies:
-- ~ redo-always
-- % redo-ifcreate
-- @ redo-ifchange
#define ifchange_dependency_prepend '@'
#define ifcreate_dependency_prepend '%'
#define always_dependency_prepend '~'

-- TODO make the interface to this file only have absolute paths
-- consider making it abstracted by token or something?

-- TODO instead of metaDepsDir, maybe we can simplify this and just call it the target's db entry or something..?
-- then we should get rid of any function referencing by target. Everything should be given a metadeps dir instead?

---------------------------------------------------------------------
-- Functions initializing the meta directory for a target
---------------------------------------------------------------------
-- Directory for storing and fetching data on dependencies of redo targets.
redoMetaDir :: IO FilePath
redoMetaDir = getAppUserDataDirectory "redo"

-- Form the hash directory where a target's dependency hashes will be stored given the target
metaFileDir :: Target -> IO MetaDir
metaFileDir target = do
  metaRoot <- redoMetaDir 
  hashedTarget <- hashString target
  return $ MetaDir $ metaRoot </> pathify hashedTarget
  where 
    pathify "" = ""
    pathify string = x </> pathify xs
      where (x,xs) = splitAt 2 string

-- Create a hash string for a target:
-- TODO remove canonicalize Path here
hashString :: Target -> IO FilePath
hashString target = do 
  absPath <- canonicalizePath' $ unTarget target
  return $ hex $ BS.unpack $ hash $ BS.pack absPath

-- Create meta data folder for storing hashes and/or timestamps and return the folder name
-- We store a dependency for the target on the do file
-- Note: this function also blows out the old directory, which is good news because we don't want old
-- dependencies hanging around if we are rebuilding a file.
initializeMetaDepsDir :: Target -> DoFile -> IO MetaDir
initializeMetaDepsDir target doFile = do
  metaDepsDir <- metaFileDir target
  initializeMetaDepsDir' metaDepsDir doFile
  return metaDepsDir

initializeMetaDepsDir' :: MetaDir -> DoFile -> IO ()
initializeMetaDepsDir' metaDepsDir doFile = do
  removeMetaDir metaDepsDir
  createMetaDir metaDepsDir
  -- Write out .do script as dependency:
  storeHashFile metaDepsDir (Target $ unDoFile doFile) (Target $ unDoFile doFile)
  -- Cache the do file:
  cacheDoFile metaDepsDir doFile
  --putStatusStrLn $ "building meta deps for " ++ target ++ " at " ++ metaDepsDir

-- Cache the do file path so we know which do was used to build a target the last time it was built
cacheDoFile :: MetaDir -> DoFile -> IO ()
cacheDoFile metaDepsDir doFile = writeFile (unMetaFile $ doFileCache metaDepsDir) (unDoFile doFile)

---------------------------------------------------------------------
-- Functions querying the meta directory for a target
---------------------------------------------------------------------
-- Retrieve the cached do file path inside meta dir
getCachedDoFile :: MetaDir -> IO (Maybe DoFile)
getCachedDoFile metaDepsDir = catch (readCache cache) (\(_ :: SomeException) -> return Nothing)
  where readCache cachedDo = do doFile <- readFile cachedDo
                                return $ Just $ DoFile doFile
        cache = unMetaFile $ doFileCache metaDepsDir

-- Returns the path to the target, if it exists, otherwise it returns the path to the
-- phony target if it exists, else return Nothing
getBuiltTargetPath :: Target -> IO(Maybe Target)
getBuiltTargetPath target = returnTargetIfExists (returnPhonyIfExists (return Nothing) =<< phonyFile target) target
  where returnTargetIfExists failFunc file = bool failFunc (return $ Just file) =<< doesTargetExist file
        returnPhonyIfExists failFunc file = bool failFunc (return $ Just $ Target $ unMetaFile file) =<< doesMetaFileExist file

getBuiltTargetPath' :: MetaDir -> Target -> IO(Maybe Target)
getBuiltTargetPath' metaDepsDir = returnTargetIfExists (returnPhonyIfExists (return Nothing) (phonyFile' metaDepsDir))
  where returnTargetIfExists failFunc file = bool failFunc (return $ Just file) =<< doesTargetExist file
        returnPhonyIfExists failFunc file = bool failFunc (return $ Just $ Target $ unMetaFile file) =<< doesMetaFileExist file

---------------------------------------------------------------------
-- Functions for creating and destroying lock files
---------------------------------------------------------------------
removeLockFiles :: IO ()
removeLockFiles = do dir <- redoMetaDir 
                     safeRemoveGlob dir ".lck.*.lck."

-- Return the lock file name for a target:
createLockFile :: Target -> IO LockFile
createLockFile target = do dir <- redoMetaDir
                           hashedTarget <- hashString target
                           return $ LockFile $ dir </> ".lck." ++ hashedTarget ++ ".lck."
                           
---------------------------------------------------------------------
-- Functions checking if a target or its dependencies are up to date
---------------------------------------------------------------------
-- Top upToDate which should be called by redo-ifchange. Return true if a file is clean and does
-- not need to be built. Return false if a file is dirty and needs to be rebuilt.
-- Note: target must be the absolute canonicalized path to the target
upToDate :: Target -> IO Bool
upToDate target = do
  depDir <- metaFileDir target
  return () `debug'` "=checking"
  hasMetaDeps <- doesMetaDirExist depDir
  targetExists <- doesTargetExist target
  case (targetExists, hasMetaDeps) of 
    -- If no meta data for this target is stored and it doesn't exist than it has never been built
    (False, False) -> return False `debug'` "-never built"
    -- If the target exists on the filesystem but does not have meta deps dir then redo never
    -- created it. It must be a source file so it is up to date.
    (True, False) -> return True `debug'` "+source"
    -- If the meta deps dir exists, then we need to check extra info contained within it to determine
    -- if the target is up to date:
    (_, True) -> do
      existingTarget <- getBuiltTargetPath' depDir target
      -- If neither a target or a phony target exists, then the target is obviously not up to date
      if isNothing existingTarget then returnFalse depDir `debug'` "-not built"
      else do
        dirty <- isTargetMarkedDirty depDir
        -- If we have already checked off this target as dirty, don't delay, return not up to date
        if dirty then return False `debug'` "-dirty"
        else do
          clean <- isTargetMarkedClean depDir
          -- If we have already checked off this target as up to date, there is no need to check again
          if clean then return True `debug'` "+clean"
          else do 
            cachedTimeStamp <- getTargetBuiltTimeStamp depDir
            currentTimeStamp <- safeGetTargetTimeStamp target
            whenTargetNotModified cachedTimeStamp currentTimeStamp 
              (return False `debug'` "-modified") 
              (upToDate' 0 target depDir)
  where
    -- Convenient debug function:
    debug' = debugUpToDate 0 target
    
upToDate' :: Int -> Target -> MetaDir -> IO Bool
upToDate' level target depDir = do
  doFile <- findDoFile target
  -- If no do file is found, but the meta dir exists, than this file used to be buildable, but is
  -- now a newly marked source file. So remove the meta dir but return false to be conservative. 
  -- There is no need to mark the file clean because the meta dir is removed.
  if isNothing doFile then (removeMetaDir depDir >> return False) `debug'` "+new source"
  else do
    let absDoFile = fromJust doFile
    newDo <- newDoFile depDir absDoFile
    -- If the target exists but a new do file was found for it then we need to rebuilt it, so
    -- it is not up to date.
    if newDo then returnFalse depDir `debug'` "-new .do"
    else do
      let doFileDir = takeDirectory $ unDoFile absDoFile
      -- If all of the dependencies are up to date then this target is also up to date, so mark it
      -- as such and return true. Else, return false.
      depsClean <- depsUpToDate (level+1) target depDir doFileDir 
      if depsClean then returnTrue depDir -- `debug'` "+deps clean"
      else returnFalse depDir -- `debug'` "-deps dirty "
  where 
    debug' = debugUpToDate level target
    -- Does the target have a new do file from the last time it was built?
    newDoFile :: MetaDir -> DoFile -> IO Bool
    newDoFile metaDepsDir doFile =
      -- We shouldn't expect a do file to build another do file by default, so skip this check
      -- otherwise we end up with uncorrect behavior
      if takeExtension (unTarget target) == ".do" then return False
      else maybe (return True) (pathsNotEqual doFile) =<< getCachedDoFile metaDepsDir
      where pathsNotEqual path1 path2 = if path1 /= path2 then return True else return False

-- Are a target's redo-create or redo-always or redo-ifchange dependencies up to date? 
-- If so return, true, otherwise return false. Note that this function recurses on a target's
-- dependencies to make sure the dependencies are up to date.
depsUpToDate :: Int -> Target -> MetaDir -> FilePath ->  IO Bool
depsUpToDate level target metaDepsDir doFileDir = do
  depHashFiles <- getMetaDirContents metaDepsDir
  if anyAlwaysDeps depHashFiles then return False `debug'` "-dep always"
  else do 
    -- redo-ifcreate - if one of those files was created, we need to return False immediately
    depCreated' <- mapOr (depCreated . Target . unEscapeIfCreatePath . unMetaFile) (ifCreateDeps depHashFiles)
    if depCreated' then return False `debug'` "-dep created"
    -- redo-ifchange - check these files hashes against those stored to determine if they are up to date
    --                 then recursively check their dependencies to see if they are up to date
    else mapAnd (ifChangeDepsUpToDate level metaDepsDir doFileDir) (ifChangeDeps depHashFiles)
  where 
    debug' = debugUpToDate level target
    -- Returns true if there are any "-always" dependencies present:
    anyAlwaysDeps = any (fileHasPrepend always_dependency_prepend) 
    -- Functions which filter a set of dependencies for only those made with "-ifchange" or "-ifcreate"
    ifChangeDeps = filter (fileHasPrepend ifchange_dependency_prepend)
    ifCreateDeps = filter (fileHasPrepend ifcreate_dependency_prepend)
    -- Check if dep file begins with certain prepend string
    fileHasPrepend depPrepend metaFile = take 2 (unMetaFile metaFile) == ['.'] ++ [depPrepend]
    -- Has a dependency been created
    -- TODO: This shouldn't be run if the file already was created, just the first time it was created.
    depCreated :: Target -> IO Bool
    depCreated = doesTargetExist

-- Are a target's redo-ifchange dependencies up to date?
ifChangeDepsUpToDate :: Int -> MetaDir -> FilePath -> MetaFile -> IO Bool
ifChangeDepsUpToDate level parentMetaDir doDir hashFile = do
  depDir <- metaFileDir dep
  return () `debug'` "=checking"
  hasMetaDeps <- doesMetaDirExist depDir
  targetExists <- doesTargetExist dep
  case (targetExists, hasMetaDeps) of 
    -- If no meta data for this target is stored and it doesn't exist than it has never been built
    (False, False) -> return False `debug'` "-never built"
    -- If the target exists on the filesystem but does not have meta deps dir then redo never
    -- created it. It must be a source file so we need to check its stamp
    (True, False) -> do hashesMatch <- compareStamp hashFullPath dep 
                        if hashesMatch then return True `debug'` "+unchanged"
                        else return False `debug'` "-changed"                              
    -- If the meta deps dir exists, then we need to check extra info contained within it to determine
    -- if the target is up to date:
    (_, True) -> do
      existingTarget <- getBuiltTargetPath' depDir dep
      -- If neither a target or a phony target exists, then the target is obviously not up to date
      if isNothing existingTarget then returnFalse depDir `debug'` "-not built"
      else do
        dirty <- isTargetMarkedDirty depDir
        -- If we have already checked off this target as dirty, don't delay, return not up to date
        if dirty then return False `debug'` "-dirty"
        else do
          clean <- isTargetMarkedClean depDir
          -- If we have already checked off this target as up to date, there is no need to check again
          if clean then return True `debug'` "+clean"
          else do 
            cachedTimeStamp <- getTargetBuiltTimeStamp depDir
            currentTimeStamp <- safeGetTargetTimeStamp dep
            whenTargetNotModified cachedTimeStamp currentTimeStamp (return False `debug'` "-modified") (do
              -- Check the target against it's stored hash
              hashesMatch <- compareStamp hashFullPath (fromJust existingTarget)
              if hashesMatch then upToDate' level dep depDir
              else return False `debug'` "-dep changed")
  where
    debug' = debugUpToDate level dep
    hashFilePath = unMetaFile hashFile
    hashFullPath = MetaFile $ unMetaDir parentMetaDir </> hashFilePath
    dep = Target $ removeDotDirs $ doDir </> unEscapeIfChangePath hashFilePath
    -- Check the hash of the dependency and compare it to the stored hash. This function provides recursion:
    compareStamp :: MetaFile -> Target -> IO Bool
    compareStamp storedStamp fileToStamp = do
      oldStamp <- readMetaFile storedStamp
      newStamp <- getTargetStamp fileToStamp
      return $ oldStamp == newStamp

-- Helper function which returns true and marks the target as clean:
returnTrue :: MetaDir -> IO Bool
returnTrue metaDepsDir = markTargetClean metaDepsDir >> return True
-- Helper function which returns false and marks the target as dirty:
returnFalse :: MetaDir -> IO Bool
returnFalse metaDepsDir = markTargetDirty metaDepsDir >> return False

-- Helper for debugging:
debugUpToDate :: Int -> Target -> c -> String -> c
debugUpToDate depth file a string = debug a (createSpaces (depth*2) ++ string ++ createSpaces paddingToAppend ++ " -- " ++ unTarget file)
  where createSpaces num = concat $ replicate num " "
        stringWidth = 12
        paddingToAppend = stringWidth - length string
                
safeGetTargetTimeStamp :: Target -> IO (Maybe Stamp)
safeGetTargetTimeStamp target = catch (Just <$> getTargetTimeStamp target) (\(_ :: SomeException) -> return Nothing)

-- Run an action if the target was not modified outside of redo
whenTargetNotModified :: Maybe Stamp -> Maybe Stamp -> t -> t -> t
whenTargetNotModified previousTimeStamp currentTimeStamp failAction action =
  -- Make sure that the user didn't modify the target file outside of redo, we don't want to clobber user changes.
  -- Get the last time the target was built by redo:
  if isNothing previousTimeStamp || 
     isNothing currentTimeStamp || 
     currentTimeStamp == previousTimeStamp then action
  else failAction

-- Returns the absolute path to the do file given the absolute path to the target:
findDoFile :: Target -> IO (Maybe DoFile)
findDoFile absTarget = do 
  let (targetDir, targetName) = splitFileName $ unTarget absTarget
  let targetDo = DoFile $ removeDotDirs $ unTarget absTarget ++ ".do" 
  bool (defaultDoPath targetDir targetName) (return $ Just targetDo) =<< doesFileExist (unDoFile targetDo)
  where
    -- Try to find matching .do file by checking directories upwards of "." until a suitable match is 
    -- found or "/" is reached.
    defaultDoPath :: FilePath -> FilePath -> IO (Maybe DoFile)
    defaultDoPath absPath' name = do
      let absPath = if last absPath' == pathSeparator then takeDirectory absPath' else absPath'
      doFile <- listToMaybe `liftM` filterM doesFileExist (candidates absPath name)
      if isNothing doFile && not (isDrive absPath) then defaultDoPath (takeDirectory absPath) name
      else returnDoFile doFile
    -- Return nothing or return the properly wrapped do file path:
    returnDoFile :: Maybe FilePath -> IO (Maybe DoFile)
    returnDoFile doFile = if isNothing doFile then return Nothing
                          else return $ Just $ DoFile $ fromJust doFile
    -- List the possible default.do file candidates relative to the given path:
    candidates path name = map (path </>) (defaults name)
    defaults name = map (++ ".do") (getDefaultDo $ "default" ++ takeExtensions name)
    -- Form all possible matching default.do files in order of preference:
    getDefaultDo :: FilePath -> [FilePath]
    getDefaultDo filename = filename : if smallfilename == filename then [] else getDefaultDo $ dropFirstExtension filename
      where smallfilename = dropExtension filename
            basefilename = dropExtensions filename
            dropFirstExtension fname = basefilename ++ takeExtensions (drop 1 (takeExtensions fname))

---------------------------------------------------------------------
-- Functions for marking dependencies as clean or dirty
---------------------------------------------------------------------
-- Store a file to signify that this file has been checked, and is up
-- to date or not up to date for this session.
markTargetClean :: MetaDir -> IO ()
markTargetClean metaDepsDir = do
  removeSessionFiles metaDepsDir
  createEmptyMetaFile =<< cleanFile metaDepsDir
markTargetDirty :: MetaDir -> IO ()
markTargetDirty metaDepsDir = do
  removeSessionFiles metaDepsDir
  createEmptyMetaFile =<< dirtyFile metaDepsDir
markTargetBuilt :: Target -> MetaDir -> IO ()
markTargetBuilt target metaDepsDir = do
  timestamp <- getTargetTimeStamp target
  writeMetaFile (builtFile metaDepsDir) timestamp

-- Check for stored clean or dirty files in a meta dir.
isTargetMarkedClean :: MetaDir -> IO Bool 
isTargetMarkedClean metaDepsDir = doesMetaFileExist =<< cleanFile metaDepsDir
isTargetMarkedDirty :: MetaDir -> IO Bool 
isTargetMarkedDirty metaDepsDir = doesMetaFileExist =<< dirtyFile metaDepsDir

-- Construct filename for storing clean / dirty in a meta dir.
cleanFile :: MetaDir -> IO MetaFile  
cleanFile metaDepsDir = f metaDepsDir =<< getEnv "REDO_SESSION"
  where f depDir session = return $ MetaFile $ unMetaDir depDir </> ".cln." ++ session  ++ ".cln."
dirtyFile :: MetaDir -> IO MetaFile 
dirtyFile metaDepsDir = f metaDepsDir =<< getEnv "REDO_SESSION"
  where f depDir session = return $ MetaFile $ unMetaDir depDir </> ".drt." ++ session  ++ ".drt."

-- Construct file for storing built timestamp
builtFile :: MetaDir -> MetaFile  
builtFile metaDepsDir = MetaFile $ unMetaDir metaDepsDir </> ".blt.blt."

-- Remove dirty and clean files in a meta dir.
removeSessionFiles :: MetaDir -> IO ()
removeSessionFiles metaDepsDir = safeRemoveGlob metaDepsDir' ".cln.*.cln." >> safeRemoveGlob metaDepsDir' ".drt.*.drt." 
  where metaDepsDir' = unMetaDir metaDepsDir

---------------------------------------------------------------------
-- Functions writing dependency files
---------------------------------------------------------------------
-- Calculate the hash of a file. If the file is a directory,
-- then return the timestamp instead.
-- TODO: implement timestamps, also make hash stored as binary
getTargetStamp :: Target -> IO Stamp 
getTargetStamp file = do 
  --isDir <- doesDirectoryExist file
  --if isDir then do
  --  timestamp <- getModificationTime file
  --  return $ BS.pack $ show timestamp
  --else do
  --  timestamp <- getModificationTime file
  --  return $ BS.pack $ show timestamp
     -- hash `liftM` BS.readFile file
  --------------------------------------------
  getTargetTimeStamp file

-- Hash the file
-- FIXME
-- getTargetHashStamp :: Target -> IO Stamp
-- getTargetHashStamp file = Stamp <$> hash `liftM` unStamp <$> readMetaFile (unTarget file)

-- Get the file timestamp
getTargetTimeStamp :: Target -> IO Stamp
getTargetTimeStamp file = do
  st <- getFileStatus $ unTarget file
  return $ Stamp $ BS.pack $ show (modificationTimeHiRes st) ++ show (fileID st) ++ show (fileSize st)

-- Get the cached timestamp for when a target was last built. Return '.'
getTargetBuiltTimeStamp :: MetaDir -> IO (Maybe Stamp)
getTargetBuiltTimeStamp metaDepsDir = catch (Just <$> readMetaFile (builtFile metaDepsDir)) 
  (\(_ :: SomeException) -> return Nothing)

-- Calculate the hash of a target's dependency and write it to the proper meta data location
-- If the dependency doesn't exist, do not store a hash
writeMetaFile :: MetaFile -> Stamp -> IO ()
writeMetaFile file contents = catch
  ( BS.writeFile fileToWrite byteContents )
  (\(_ :: SomeException) -> do cd <- getCurrentDirectory 
                               putErrorStrLn $ "Error: Encountered problem writing '" ++ BS.unpack byteContents ++ "' to '" ++ cd </> fileToWrite ++ "'."
                               exitFailure)
  where byteContents = unStamp contents
        fileToWrite = unMetaFile file

readMetaFile :: MetaFile -> IO Stamp
readMetaFile file = Stamp <$> BS.readFile (unMetaFile file)

-- Creation of an empty dep file for redo-always and redo-ifcreate
-- note may need to make specific one for redoifcreate and redoalways
createEmptyMetaFile :: MetaFile -> IO ()
createEmptyMetaFile file = writeMetaFile file (Stamp $ BS.singleton '.')

-- Store dependencies for redo-ifchange:
storeIfChangeDependencies :: [Target] -> IO ()
storeIfChangeDependencies = storeDependencies storeIfChangeDep

-- Store dependencies for redo-ifcreate:
storeIfCreateDependencies :: [Target] -> IO ()
storeIfCreateDependencies = storeDependencies storeIfCreateDep

-- Return some redo environment vaiables
getRedoEnv :: IO (FilePath, MetaDir)
getRedoEnv = do
  parentRedoPath <- getEnv "REDO_PATH" -- directory where .do file was run from
  parentRedoTarget <- getEnv "REDO_TARGET"
  parentRedoMetaDir <- metaFileDir $ Target parentRedoTarget
  return (parentRedoPath, parentRedoMetaDir)

-- Store dependency for redo-always:
storeAlwaysDependency :: IO ()
storeAlwaysDependency = do 
  (parentRedoPath, parentRedoMetaDir) <- getRedoEnv
  performActionInDir parentRedoPath storeAlwaysDep parentRedoMetaDir

-- Store dependencies given a store action and a list of dependencies to store:
storeDependencies :: (MetaDir -> Target -> IO ()) -> [Target] -> IO ()  
storeDependencies storeAction dependencies = do 
  (parentRedoPath, parentRedoMetaDir) <- getRedoEnv
  dependenciesRel2Parent <- makeRelativeToParent parentRedoPath dependencies 
  mapM_ (performActionInDir parentRedoPath (storeAction parentRedoMetaDir)) dependenciesRel2Parent
  where
    makeRelativeToParent :: FilePath -> [Target] -> IO [Target]
    makeRelativeToParent parent targets = do
      currentDir <- getCurrentDirectory
      -- Note: All target listed here are relative to the current directory in the .do script. This could
      -- be different than the REDO_PATH variable, which represents the directory where the .do was invoked 
      -- if 'cd' was used in the .do script.
      -- So, let's get a list of targets relative to the parent .do file invocation location, REDO_PATH
      return $ map (Target . makeRelative parent . (currentDir </>) . unTarget) targets

-- This applies a function to a target in the directory provided and then
-- returns the current directory to the starting directory:
-- TODO: do we still need this?
performActionInDir :: FilePath -> (t -> IO ()) -> t -> IO ()
performActionInDir dir action target = do
  --topDir <- getCurrentDirectory
  --catch (setCurrentDirectory dir) (\(_ :: SomeException) -> do 
  --  putErrorStrLn $ "Error: No such directory " ++ topDir </> dir
  --  exitFailure)
  action target
  --setCurrentDirectory topDir

-- Store the stamp of a dependency if it exists. If it does not exist, then we store a blank stamp file
-- because the target still depenends on this target, it just failed to built last time, so we store a
-- blank (bad) stamp that will never match a successfully built target
storeIfChangeDep :: MetaDir -> Target -> IO ()
storeIfChangeDep metaDepsDir dep = maybe (createEmptyMetaFile theMetaFile) (storeHashFile metaDepsDir dep) =<< getBuiltTargetPath dep
  where theMetaFile = ifChangeMetaFile metaDepsDir dep

storeIfCreateDep :: MetaDir -> Target -> IO ()
storeIfCreateDep metaDepsDir dep = createEmptyMetaFile $ ifCreateMetaFile metaDepsDir dep

storeAlwaysDep :: MetaDir -> IO ()
storeAlwaysDep metaDepsDir = createEmptyMetaFile $ alwaysMetaFile' metaDepsDir

storePhonyTarget :: MetaDir -> IO () 
storePhonyTarget metaDepsDir = createEmptyMetaFile $ phonyFile' metaDepsDir

storeHashFile :: MetaDir -> Target -> Target -> IO ()
storeHashFile metaDepsDir depName depToHash = writeMetaFile theMetaFile =<< getTargetStamp depToHash
  where theMetaFile = ifChangeMetaFile metaDepsDir depName

---------------------------------------------------------------------
-- Functions creating file names for storing dependencies
---------------------------------------------------------------------
-- Form the hash file path for a target's dependency given the current target meta dir and the target's dependency
getMetaFile :: (FilePath -> FilePath) -> MetaDir -> Target -> MetaFile
getMetaFile escapeFunc metaDir dependency = MetaFile $ unMetaDir metaDir </> escapeFunc (unTarget dependency)

-- Functions to get the dependency path for each file type
ifChangeMetaFile :: MetaDir -> Target -> MetaFile 
ifChangeMetaFile = getMetaFile escapeIfChangePath
ifCreateMetaFile :: MetaDir -> Target -> MetaFile 
ifCreateMetaFile = getMetaFile escapeIfCreatePath
alwaysMetaFile' :: MetaDir -> MetaFile 
alwaysMetaFile' depDir = MetaFile $ unMetaDir depDir </> file
  where file = "." ++ [always_dependency_prepend] ++ "redo-always" ++ [always_dependency_prepend] ++ "."

phonyFile :: Target -> IO MetaFile
phonyFile target = do depDir <- metaFileDir target 
                      return $ phonyFile' depDir
phonyFile' :: MetaDir -> MetaFile
phonyFile' metaDepsDir = MetaFile $ unMetaDir metaDepsDir </> "." ++ "phony-target" ++ "."

doFileCache :: MetaDir -> MetaFile  
doFileCache metaDepsDir = MetaFile $ unMetaDir metaDepsDir </> ".do.do."

---------------------------------------------------------------------
-- Functions escaping and unescaping path names
---------------------------------------------------------------------
-- This is the same as running normalise, but it always removes the trailing path
-- separator, and it always keeps a "./" in front of things in the current directory
-- and always removes "./" in front of things not in the current directory.
-- we use this to ensure consistancy of naming convention
sanitizeFilePath :: FilePath -> FilePath
sanitizeFilePath filePath = normalise $ dir </> file
  where (dir, file) = splitFileName . dropTrailingPathSeparator . normalise $ filePath

-- Takes a file path and replaces all </> with @
escapeDependencyPath :: Char -> FilePath -> FilePath
escapeDependencyPath dependency_prepend path = (['.'] ++ [dependency_prepend]) ++ concatMap repl path' ++ ([dependency_prepend] ++ ['.'])
  where path' = sanitizeFilePath path
        repl seperator_replacement = [seperator_replacement] ++ [seperator_replacement_escape]
        repl c   = if isPathSeparator c then [seperator_replacement] else [c]

-- Reverses escapeFilePath
unEscapeDependencyPath :: Char -> FilePath -> FilePath
unEscapeDependencyPath dependency_prepend name = sanitizeFilePath path
  where 
    path = if take 2 name == (['.'] ++ [dependency_prepend]) then unEscape $ (dropEnd 2 . drop 2) name else name
    dropEnd n list = take (length list - n) list
    unEscape [] = []
    unEscape string = first : unEscape rest
      where
        (first, rest) = repl string
        repl [] = ('\0',"")
        repl (x:xs) = if x == seperator_replacement
                      then if head xs == seperator_replacement_escape
                           then (seperator_replacement, tail xs)
                           else (pathSeparator, xs)
                      else (x, xs)

-- Functions to escape and unescape dependencies of different types:
escapeIfChangePath :: FilePath -> FilePath
escapeIfChangePath = escapeDependencyPath ifchange_dependency_prepend
unEscapeIfChangePath :: FilePath -> FilePath 
unEscapeIfChangePath = unEscapeDependencyPath ifchange_dependency_prepend
escapeIfCreatePath :: FilePath -> FilePath
escapeIfCreatePath = escapeDependencyPath ifcreate_dependency_prepend
unEscapeIfCreatePath :: FilePath -> FilePath 
unEscapeIfCreatePath = unEscapeDependencyPath ifcreate_dependency_prepend

---------------------------------------------------------------------
-- Functions for querying about a Target
---------------------------------------------------------------------
-- Does the target file or directory exist on the filesystem?
-- Checks if a target file is a buildable target, or if it is a source file
isSourceFile :: Target -> IO Bool
isSourceFile target = bool (return False) (not <$> hasDependencies target) =<< doesTargetExist target
  where
    -- Check's if a target has dependencies stored already
    hasDependencies :: Target -> IO Bool
    hasDependencies t = doesMetaDirExist =<< metaFileDir t
  
doesTargetExist :: Target -> IO Bool
doesTargetExist target = (||) <$> doesFileExist filePath <*> doesDirectoryExist filePath
  where filePath = unTarget target

---------------------------------------------------------------------
-- Functions acting on MetaDir
---------------------------------------------------------------------
doesMetaDirExist :: MetaDir -> IO Bool
doesMetaDirExist depDir = doesDirectoryExist $ unMetaDir depDir

doesMetaFileExist :: MetaFile -> IO Bool
doesMetaFileExist metaFile = doesFileExist $ unMetaFile metaFile

removeMetaDir :: MetaDir -> IO ()
removeMetaDir dir = catchJust (guard . isDoesNotExistError) (removeDirectoryRecursive $ unMetaDir dir) (\_ -> return())

createMetaDir :: MetaDir -> IO ()
createMetaDir dir = createDirectoryIfMissing True (unMetaDir dir)

getMetaDirContents :: MetaDir -> IO [MetaFile]
getMetaDirContents dir = do contents <- getDirectoryContents $ unMetaDir dir
                            return $ map MetaFile contents
