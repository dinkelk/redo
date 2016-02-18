{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database(metaDir, initializeMetaDepsDir, isSourceFile, storeIfChangeDependencies, storeIfCreateDependencies, 
                storeAlwaysDependency, upToDate, storePhonyTarget, createLockFile, removeLockFiles, 
                markTargetClean, markTargetDirty) where

import Control.Applicative ((<$>))
import Control.Exception (catch, SomeException(..))
import qualified Data.ByteString.Char8 as BS
import Crypto.Hash.MD5 (hash) 
import Data.Hex (hex)
import Data.Bool (bool)
import Data.Maybe (isNothing, fromJust)
import System.Directory (getAppUserDataDirectory, doesFileExist, getDirectoryContents, createDirectoryIfMissing, getCurrentDirectory, doesDirectoryExist)
import System.FilePath (normalise, dropTrailingPathSeparator, makeRelative, splitFileName, (</>), takeDirectory, isPathSeparator, pathSeparator, takeExtension)
import System.Environment (getEnv)
import System.Exit (exitFailure)
import System.Posix.Files (getFileStatus, modificationTimeHiRes, fileID, fileSize)

import PrettyPrint
import Helpers

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
metaDir :: IO (String)
metaDir = getAppUserDataDirectory "redo"

-- Form the hash directory where a target's dependency hashes will be stored given the target
depFileDir :: FilePath -> IO (FilePath)
depFileDir target = do
  metaRoot <- metaDir 
  hashedTarget <- hashString target
  return $ metaRoot </> (pathify hashedTarget)
  where 
    pathify "" = ""
    pathify string = x </> pathify xs
      where (x,xs) = splitAt 2 string

-- Create a hash string for a target:
-- TODO remove canonicalize Path here
hashString :: FilePath -> IO (FilePath)
hashString target = do 
  absPath <- canonicalizePath' target
  return $ hex $ BS.unpack $ hash $ BS.pack absPath

-- Create meta data folder for storing hashes and/or timestamps and return the folder name
-- We store a dependency for the target on the do file
-- Note: this function also blows out the old directory, which is good news because we don't want old
-- dependencies hanging around if we are rebuilding a file.
initializeMetaDepsDir :: FilePath -> FilePath -> IO (FilePath)
initializeMetaDepsDir target doFile = initDir =<< depFileDir target
  where initDir metaDepsDir = do
          safeRemoveDirectoryRecursive metaDepsDir
          createDirectoryIfMissing True metaDepsDir 
          -- Write out .do script as dependency:
          storeHashFile metaDepsDir doFile doFile
          -- Cache the do file:
          cacheDoFile metaDepsDir doFile
          --putStatusStrLn $ "building meta deps for " ++ target ++ " at " ++ metaDepsDir
          return metaDepsDir

-- Cache the do file path so we know which do was used to build a target the last time it was built
cacheDoFile :: FilePath -> FilePath -> IO ()
cacheDoFile metaDepsDir absoluteDoFile = writeFile (metaDepsDir </> ".do.do.") absoluteDoFile

---------------------------------------------------------------------
-- Functions querying the meta directory for a target
---------------------------------------------------------------------
-- Retrieve the cached do file path inside meta dir
getCachedDoFile :: FilePath -> IO (Maybe FilePath)
getCachedDoFile metaDepsDir = bool (return Nothing) (readCache doFileCache) =<< doesFileExist doFileCache
  where doFileCache = metaDepsDir </> ".do.do."
        readCache cachedDo = do doFile <- readFile cachedDo
                                return $ Just doFile

-- Returns the path to the target, if it exists, otherwise it returns the path to the
-- phony target if it exists, else return Nothing
getBuiltTargetPath :: FilePath -> IO(Maybe FilePath)
getBuiltTargetPath target = returnTargetIfExists (returnTargetIfExists (return Nothing) =<< phonyFile target) target
  where returnTargetIfExists failFunc file = bool (failFunc) (return $ Just file) =<< doesTargetExist file

-- Checks if a target file is a buildable target, or if it is a source file
-- TODO optimize this call
isSourceFile :: FilePath -> IO Bool
isSourceFile target = bool (return False) (not <$> hasDependencies target) =<< doesTargetExist target
  where
    -- Check's if a target has dependencies stored already
    hasDependencies :: FilePath -> IO Bool
    hasDependencies t = doesDirectoryExist =<< depFileDir t
  
---------------------------------------------------------------------
-- Functions for creating and destroying lock files
---------------------------------------------------------------------
removeLockFiles :: IO ()
removeLockFiles = do dir <- metaDir
                     safeRemoveGlob dir ".lck.*.lck."

-- Return the lock file name for a target:
createLockFile :: FilePath -> IO (FilePath)
createLockFile target = do dir <- metaDir
                           hashedTarget <- hashString target
                           return $ dir </> ".lck." ++ hashedTarget ++ ".lck."
                           
---------------------------------------------------------------------
-- Functions checking if a target or its dependencies are up to date
---------------------------------------------------------------------
-- Top upToDate which should be called by redo-ifchange. Return true if a file is clean and does
-- not need to be built. Return false if a file is dirty and needs to be rebuilt.
-- Note: target must be the absolute canonicalized path to the target
upToDate :: FilePath -> IO Bool
upToDate target = do
  depDir <- depFileDir target
  return () `debug'` "=checking"
  hasMetaDeps <- doesDirectoryExist depDir
  targetExists <- doesTargetExist target
  case (targetExists, hasMetaDeps) of 
    -- If no meta data for this target is stored and it doesn't exist than it has never been built
    (False, False) -> return False `debug'` "+not built"
    -- If the target exists on the filesystem but does not have meta deps dir then redo never
    -- created it. It must be a source file so it is up to date.
    (True, False) -> return True `debug'` "+source"
    -- If the meta deps dir exists, then we need to check extra info contained within it to determine
    -- if the target is up to date:
    (_, True) -> do
      dirty <- isTargetMarkedDirty depDir
      -- If we have already checked off this target as dirty, don't delay, return not up to date
      if dirty then return False `debug'` "-dirty"
      else do
        clean <- isTargetMarkedClean depDir
        -- If we have already checked off this target as up to date, there is no need to check again
        if clean then return True `debug'` "+clean"
        else do 
          let phonyTarget = phonyFile' depDir
          phonyTargetExists <- doesFileExist phonyTarget
          let existingTarget = if targetExists then target
                               else if phonyTargetExists then phonyTarget else ""
          -- If neither a target or a phony target exists, then the target is obviously not up to date
          if null existingTarget then (returnFalse depDir) `debug'` "-not built"
          else upToDate' 0 target depDir
  where
    -- Convenient debug function:
    debug' a b = debugUpToDate 0 target a b
    
upToDate' :: Int -> FilePath -> FilePath -> IO Bool
upToDate' level target depDir = do
  doFile <- findDoFile target
  -- If no do file is found, but the meta dir exists, than this file used to be buildable, but is
  -- now a newly marked source file. So remove the meta dir but return false to be conservative. 
  -- There is no need to mark the file clean because the meta dir is removed.
  if isNothing doFile then (safeRemoveDirectoryRecursive depDir >> return False) `debug'` "+new source"
  else do
    let absDoFile = fromJust doFile
    newDo <- newDoFile depDir absDoFile
    -- If the target exists but a new do file was found for it then we need to rebuilt it, so
    -- it is not up to date.
    if newDo then (returnFalse depDir) `debug'` "-new .do"
    else do
      let doFileDir = takeDirectory absDoFile
      -- If all of the dependencies are up to date then this target is also up to date, so mark it
      -- as such and return true. Else, return false.
      depsClean <- depsUpToDate (level+1) target depDir doFileDir 
      if depsClean then (returnTrue depDir) -- `debug'` "+deps clean"
      else (returnFalse depDir) -- `debug'` "-deps dirty "
  where 
    debug' a b = debugUpToDate level target a b
    -- Does the target have a new do file from the last time it was built?
    newDoFile :: FilePath -> FilePath -> IO Bool
    newDoFile metaDepsDir doFile =
      -- We shouldn't expect a do file to build another do file by default, so skip this check
      -- otherwise we end up with uncorrect behavior
      if (takeExtension target) == ".do" then return False
      else do
        maybe (return True) (pathsNotEqual doFile) =<< getCachedDoFile metaDepsDir
      where pathsNotEqual path1 path2 = if path1 /= path2 then return True else return False

-- Are a target's redo-create or redo-always or redo-ifchange dependencies up to date? 
-- If so return, true, otherwise return false. Note that this function recurses on a target's
-- dependencies to make sure the dependencies are up to date.
depsUpToDate :: Int -> FilePath -> FilePath -> FilePath ->  IO Bool
depsUpToDate level target metaDepsDir doFileDir = do
  depHashFiles <- getDirectoryContents metaDepsDir
  if anyAlwaysDeps depHashFiles then return False `debug'` "-dep always"
  else do 
    -- redo-ifcreate - if one of those files was created, we need to return False immediately
    depCreated' <- mapOr (depCreated . unEscapeIfCreatePath) (ifCreateDeps depHashFiles)
    if depCreated' then return False `debug'` "-dep created"
    -- redo-ifchange - check these files hashes against those stored to determine if they are up to date
    --                 then recursively check their dependencies to see if they are up to date
    else mapAnd (ifChangeDepsUpToDate level metaDepsDir doFileDir) (ifChangeDeps depHashFiles)
  where 
    debug' a b = debugUpToDate level target a b
    -- Returns true if there are any "-always" dependencies present:
    anyAlwaysDeps = any (fileHasPrepend always_dependency_prepend) 
    -- Functions which filter a set of dependencies for only those made with "-ifchange" or "-ifcreate"
    ifChangeDeps = filter (fileHasPrepend ifchange_dependency_prepend)
    ifCreateDeps = filter (fileHasPrepend ifcreate_dependency_prepend)
    -- Check if dep file begins with certain prepend string
    fileHasPrepend depPrepend xs = take 2 xs == ['.'] ++ [depPrepend]
    -- Has a dependency been created
    -- TODO: This shouldn't be run if the file already was created, just the first time it was created.
    depCreated :: FilePath -> IO Bool
    depCreated dep = id <$> doesTargetExist dep 

-- Are a target's redo-ifchange dependencies up to date?
ifChangeDepsUpToDate :: Int -> FilePath -> FilePath -> FilePath -> IO Bool
ifChangeDepsUpToDate level depsDir doDir hashFile = do
  depDir <- depFileDir dep
  return () `debug'` "=checking"
  hasMetaDeps <- doesDirectoryExist depDir
  targetExists <- doesTargetExist dep
  case (targetExists, hasMetaDeps) of 
    -- If no meta data for this target is stored and it doesn't exist than it has never been built
    (False, False) -> return False `debug'` "+not built"
    -- If the target exists on the filesystem but does not have meta deps dir then redo never
    -- created it. It must be a source file so it is up to date.
    (True, False) -> do hashesMatch <- compareHash hashFullPath dep 
                        if hashesMatch then return True `debug'` "+unchanged"
                        else return False `debug'` "-changed"                              
    -- If the meta deps dir exists, then we need to check extra info contained within it to determine
    -- if the target is up to date:
    (_, True) -> do
      dirty <- isTargetMarkedDirty depDir
      -- If we have already checked off this target as dirty, don't delay, return not up to date
      if dirty then return False `debug'` "-dirty"
      else do
        clean <- isTargetMarkedClean depDir
        -- If we have already checked off this target as up to date, there is no need to check again
        if clean then return True `debug'` "+clean"
        else do 
          let phonyTarget = phonyFile' depDir
          phonyTargetExists <- doesFileExist phonyTarget
          let existingTarget = if targetExists then dep
                               else if phonyTargetExists then phonyTarget else ""
          -- If neither a target or a phony target exists, then the target is obviously not up to date
          if null existingTarget then (returnFalse depDir) `debug'` "-not built"
          -- Check the target against it's stored hash
          else do hashesMatch <- compareHash hashFullPath existingTarget
                  if hashesMatch then upToDate' level dep depDir
                  else return False `debug'` "-dep changed"       
  where
    debug' a b = debugUpToDate level dep a b
    hashFullPath = depsDir </> hashFile
    dep = removeDotDirs $ doDir </> unEscapeIfChangePath hashFile
    -- Check the hash of the dependency and compare it to the stored hash. This function provides recursion:
    compareHash :: FilePath -> FilePath -> IO Bool
    compareHash storedHash fileToHash = do
      oldHash <- BS.readFile storedHash 
      newHash <- computeHash fileToHash 
      return $ oldHash == newHash

-- Helper function which returns true and marks the target as clean:
returnTrue :: FilePath -> IO Bool
returnTrue metaDepsDir = markTargetClean metaDepsDir >> return True
-- Helper function which returns false and marks the target as dirty:
returnFalse :: FilePath -> IO Bool
returnFalse metaDepsDir = markTargetDirty metaDepsDir >> return False

-- Helper for debugging:
debugUpToDate :: Int -> FilePath -> c -> String -> c
debugUpToDate depth file a string = debug a (createSpaces (depth*2) ++ string ++ createSpaces paddingToAppend ++ " -- " ++ file)
  where createSpaces num = (concat $ replicate num " ") 
        stringWidth = 12
        paddingToAppend = stringWidth - length string
                
---------------------------------------------------------------------
-- Functions for marking dependencies as clean or dirty
---------------------------------------------------------------------
-- Store a file to signify that this file has been checked, and is up
-- to date or not up to date for this session.
markTargetClean :: FilePath -> IO ()
markTargetClean metaDepsDir = do
  removeSessionFiles metaDepsDir
  createEmptyDepFile =<< cleanFile metaDepsDir
markTargetDirty :: FilePath -> IO ()
markTargetDirty metaDepsDir = do
  removeSessionFiles metaDepsDir
  createEmptyDepFile =<< dirtyFile metaDepsDir

-- Check for stored clean or dirty files in a meta dir.
isTargetMarkedClean :: FilePath -> IO Bool 
isTargetMarkedClean metaDepsDir = doesFileExist =<< cleanFile metaDepsDir
isTargetMarkedDirty :: FilePath -> IO Bool 
isTargetMarkedDirty metaDepsDir = doesFileExist =<< dirtyFile metaDepsDir

-- Construct filename for storing clean / dirty in a meta dir.
cleanFile :: FilePath -> IO (FilePath)
cleanFile metaDepsDir = f metaDepsDir =<< getEnv "REDO_SESSION"
  where f depDir session = return $ depDir </> ".cln." ++ session  ++ ".cln."
dirtyFile :: FilePath -> IO (FilePath)
dirtyFile metaDepsDir = f metaDepsDir =<< getEnv "REDO_SESSION"
  where f depDir session = return $ depDir </> ".drt." ++ session  ++ ".drt."

-- Remove dirty and clean files in a meta dir.
removeSessionFiles :: FilePath -> IO ()
removeSessionFiles metaDepsDir = safeRemoveGlob metaDepsDir ".cln.*.cln." >> safeRemoveGlob metaDepsDir ".drt.*.drt." 

---------------------------------------------------------------------
-- Functions writing dependency files
---------------------------------------------------------------------
-- Calculate the hash of a file. If the file is a directory,
-- then return the timestamp instead.
-- TODO: implement timestamps, also make hash stored as binary
computeHash :: FilePath -> IO BS.ByteString
computeHash file = do 
  --isDir <- doesDirectoryExist file
  --if isDir then do
  --  timestamp <- getModificationTime file
  --  return $ BS.pack $ show timestamp
  --else do
  --  timestamp <- getModificationTime file
  --  return $ BS.pack $ show timestamp
     -- hash `liftM` BS.readFile file
  --------------------------------------------
  st <- getFileStatus file
  return $ BS.pack $ (show $ modificationTimeHiRes st) ++ (show $ fileID st) ++ (show $ fileSize st)

-- Calculate the hash of a target's dependency and write it to the proper meta data location
-- If the dependency doesn't exist, do not store a hash
writeDepFile :: FilePath -> BS.ByteString -> IO ()
writeDepFile file contents = catch
  ( BS.writeFile file contents )
  (\(_ :: SomeException) -> do cd <- getCurrentDirectory 
                               putErrorStrLn $ "Error: Encountered problem writing '" ++ BS.unpack contents ++ "' to '" ++ cd </> file ++ "'."
                               exitFailure)

-- Creation of an empty dep file for redo-always and redo-ifcreate
-- note may need to make specific one for redoifcreate and redoalways
createEmptyDepFile :: FilePath -> IO ()
createEmptyDepFile file = writeDepFile file (BS.singleton '.')

-- Store dependencies for redo-ifchange:
storeIfChangeDependencies :: [FilePath] -> IO ()
storeIfChangeDependencies = storeDependencies storeIfChangeDep

-- Store dependencies for redo-ifcreate:
storeIfCreateDependencies :: [FilePath] -> IO ()
storeIfCreateDependencies = storeDependencies storeIfCreateDep

-- Store dependency for redo-always:
storeAlwaysDependency :: IO ()
storeAlwaysDependency = do 
  parentRedoPath <- getEnv "REDO_PATH" -- directory where .do file was run from
  parentRedoTarget <- getEnv "REDO_TARGET"
  parentRedoMetaDir <- depFileDir parentRedoTarget
  performActionInDir parentRedoPath storeAlwaysDep $ parentRedoMetaDir

-- Store dependencies given a store action and a list of dependencies to store:
storeDependencies :: (FilePath -> FilePath -> IO ()) -> [FilePath] -> IO ()  
storeDependencies storeAction dependencies = do 
  parentRedoPath <- getEnv "REDO_PATH" -- directory where .do file was run from
  parentRedoTarget <- getEnv "REDO_TARGET"
  parentRedoMetaDir <- depFileDir parentRedoTarget
  dependenciesRel2Parent <- makeRelativeToParent parentRedoPath dependencies 
  mapM_ (performActionInDir parentRedoPath (storeAction parentRedoMetaDir)) dependenciesRel2Parent
  where
    makeRelativeToParent :: FilePath -> [FilePath] -> IO ([FilePath])
    makeRelativeToParent parent targets = do
      currentDir <- getCurrentDirectory
      -- Note: All target listed here are relative to the current directory in the .do script. This could
      -- be different than the REDO_PATH variable, which represents the directory where the .do was invoked 
      -- if 'cd' was used in the .do script.
      -- So, let's get a list of targets relative to the parent .do file invocation location, REDO_PATH
      return $ map (makeRelative parent . (currentDir </>)) targets

storeIfChangeDep :: FilePath -> FilePath -> IO ()
storeIfChangeDep metaDepsDir dep = maybe (return ()) (storeHashFile metaDepsDir dep) =<< getBuiltTargetPath dep

storeIfCreateDep :: FilePath -> FilePath -> IO ()
storeIfCreateDep metaDepsDir dep = createEmptyDepFile $ ifCreateDepFile metaDepsDir dep

storeAlwaysDep :: FilePath -> IO ()
storeAlwaysDep metaDepsDir = createEmptyDepFile $ alwaysDepFile' metaDepsDir

storePhonyTarget :: FilePath -> IO ()
storePhonyTarget metaDepsDir = createEmptyDepFile $ phonyFile' metaDepsDir

storeHashFile :: FilePath -> FilePath -> FilePath -> IO ()
storeHashFile metaDepsDir depName depToHash = (writeDepFile theDepFile) =<< computeHash depToHash
  where theDepFile = ifChangeDepFile metaDepsDir depName

---------------------------------------------------------------------
-- Functions creating file names for storing dependencies
---------------------------------------------------------------------
-- Form the hash file path for a target's dependency given the current target meta dir and the target's dependency
depFile :: (FilePath -> FilePath) -> FilePath -> FilePath -> FilePath
depFile escapeFunc depDir dep = depDir </> escapeFunc dep

-- Functions to get the dependency path for each file type
ifChangeDepFile :: FilePath -> FilePath -> FilePath
ifChangeDepFile = depFile escapeIfChangePath
ifCreateDepFile :: FilePath -> FilePath -> FilePath
ifCreateDepFile = depFile escapeIfCreatePath
alwaysDepFile' :: FilePath -> FilePath
alwaysDepFile' depDir = depDir </> file
  where file = "." ++ [always_dependency_prepend] ++ "redo-always" ++ [always_dependency_prepend] ++ "."

phonyFile :: FilePath -> IO (FilePath)
phonyFile target = do depDir <- depFileDir target 
                      return $ phonyFile' depDir
phonyFile' :: FilePath -> FilePath
phonyFile' metaDepsDir = metaDepsDir </> "." ++ "phony-target" ++ "."

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

