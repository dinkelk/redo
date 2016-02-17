{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database(metaDir, initializeMetaDepsDir, isSourceFile, storeIfChangeDependencies, storeIfCreateDependencies, 
                storeAlwaysDependency, upToDate, noDoFileError, storePhonyTarget, createLockFile, removeLockFiles)  where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (guard)
import Control.Exception (catch, catchJust, SomeException(..))
import qualified Data.ByteString.Char8 as BS
import Crypto.Hash.MD5 (hash) 
import Data.Hex (hex)
import Data.Bool (bool)
import Data.Maybe (isNothing, fromJust)
import System.Directory (removeFile, canonicalizePath, getAppUserDataDirectory, doesFileExist, getDirectoryContents, removeDirectoryRecursive, createDirectoryIfMissing, getCurrentDirectory, doesDirectoryExist)
import System.Exit (exitFailure)
import System.FilePath (normalise, dropTrailingPathSeparator, makeRelative, splitFileName, (</>), takeDirectory, isPathSeparator, pathSeparator, takeExtension)
import System.IO.Error (isDoesNotExistError)
import System.Environment (getEnv)
import System.Posix.Files (getFileStatus, modificationTimeHiRes, fileID, fileSize)
import System.FilePath.Glob (globDir1, compile)

import PrettyPrint
import Helpers

-- TODO make the interface to this file only have absolute paths
-- consider making it abstracted by token or something?

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
hashString :: FilePath -> IO (FilePath)
hashString target = do 
  absPath <- canonicalizePath target
  return $ hex $ BS.unpack $ hash $ BS.pack absPath

-- Create meta data folder for storing hashes and/or timestamps
-- We store a dependency for the target on the do file
-- Note: this function also blows out the old directory, which is good news because we don't want old
-- dependencies hanging around if we are rebuilding a file.
initializeMetaDepsDir :: FilePath -> FilePath -> IO ()
initializeMetaDepsDir target doFile = f =<< depFileDir target
  where f metaDepsDir = do
          catchJust (guard . isDoesNotExistError)
                    (removeDirectoryRecursive metaDepsDir)
                    (\_ -> return())
          createDirectoryIfMissing True metaDepsDir 
          -- Write out .do script as dependency:
          storeIfChangeDep target doFile
          -- Cache the do file:
          cacheDoFile target doFile
          --putStatusStrLn $ "building meta deps for " ++ target ++ " at " ++ metaDepsDir
          
-- Cache the do file path so we know which do was used to build a target the last time it was built
cacheDoFile :: FilePath -> FilePath -> IO ()
cacheDoFile target absoluteDoFile = do dir <- depFileDir target
                                       writeFile (dir </> ".do.do.") absoluteDoFile

-- Retrieve the cached do file path
getCachedDoFile :: FilePath -> IO (Maybe FilePath)
getCachedDoFile target = do dir <- depFileDir target
                            getCachedDoFile' dir

getCachedDoFile' :: FilePath -> IO (Maybe FilePath)
getCachedDoFile' metaDepsDir = bool (return Nothing) (readCache doFileCache) =<< doesFileExist doFileCache
  where doFileCache = metaDepsDir </> ".do.do."
        readCache cachedDo = do doFile <- readFile cachedDo
                                return $ Just doFile

-- Return the lock file name for a target:
createLockFile :: FilePath -> IO (FilePath)
createLockFile target = do dir <- metaDir
                           hashedTarget <- hashString target
                           return $ dir </> ".lck." ++ hashedTarget ++ ".lck."

-- Does a phony target file exist in the meta directory for a target?
doesPhonyTargetExist :: FilePath -> IO Bool
doesPhonyTargetExist target = doesFileExist =<< phonyFile target

-- Does a target file or phony file exist?
hasTargetBeenBuilt :: FilePath -> IO Bool
hasTargetBeenBuilt target = (||) <$> doesTargetExist target <*> doesPhonyTargetExist target

-- Returns the path to the target, if it exists, otherwise it returns the path to the
-- phony target if it exists, else return Nothing
getBuiltTargetPath :: FilePath -> IO(Maybe FilePath)
getBuiltTargetPath target = returnTargetIfExists (returnTargetIfExists (return Nothing) =<< phonyFile target) target
  where returnTargetIfExists failFunc file = bool (failFunc) (return $ Just file) =<< doesTargetExist file

-- Checks if a target file is a buildable target, or if it is a source file
isSourceFile :: FilePath -> IO Bool
isSourceFile target = bool (return False) (not <$> hasDependencies target) =<< doesTargetExist target
  where
    -- Check's if a target has dependencies stored already
    hasDependencies :: FilePath -> IO Bool
    hasDependencies t = doesDirectoryExist =<< depFileDir t
  
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

-- Top upToDate which should be called by redo-ifchange. Return true if a file is clean and does
-- not need to be built. Return false if a file is dirty and needs to be rebuilt.
-- TODO... jeez can this be cleaned up?
upToDate :: FilePath -> FilePath -> IO Bool
upToDate absoluteTarget absoluteDoFile = do
  depDir <- depFileDir absoluteTarget
  (ret, builtTarget) <- getBuiltTargetIfUpToDate 0 absoluteTarget depDir 
  -- Target is not built, so just exit out with the given return code
  if null builtTarget then return ret
  -- Target is built, so we need to check the other dependencies
  else upToDate2' 0 absoluteTarget depDir absoluteDoFile
  where
    -- Convenient debug function:
    debug'' level target a string = debug a ((concat $ replicate level "  ") ++ string ++ " -- " ++ target)
    -- Function which returns true and marks the target as clean:
    returnTrue metaDepsDir = markTargetClean' metaDepsDir >> return True
    -- Function which returns false and marks the target as dirty:
    returnFalse metaDepsDir = markTargetDirty' metaDepsDir >> return False
    
    -- This function performs a bunch of checks to see if a target is a source file,
    -- or is already marked clean or dirty. If all of these checks pass, meaning we
    -- need to continue checking if the target is up to date, the built target path is
    -- returned. This might just be the target, or it could be the phony target. If
    -- a check fails, then "" will be returned as the built target and a return code will
    -- be passed indicating if the checks failed with a target is "clean" (True) or 
    -- target is "dirty" (False)
    getBuiltTargetIfUpToDate :: Int -> FilePath -> FilePath -> IO (Bool, FilePath)
    getBuiltTargetIfUpToDate level target depDir = do
      return () `debug'` "=checking   "
      hasMetaDeps <- doesDirectoryExist depDir
      targetExists <- doesTargetExist target
      case (targetExists, hasMetaDeps) of 
        -- If no meta data for this target is stored and it doesn't exist than it has never been built
        (False, False) -> return (False, "")  `debug'` "+not built  "
        -- If the target exists on the filesystem but does not have meta deps dir then redo never
        -- created it. It must be a source file so it is up to date.
        (True, False) -> return (True, "") `debug'` "+source     "
        -- If the meta deps dir exists, then we need to check extra info contained within it to determine
        -- if the target is up to date:
        (_, True) -> do
          dirty <- isTargetMarkedDirty' depDir
          -- If we have already checked off this target as dirty, don't delay, return not up to date
          if dirty then return (False, "") `debug'` "-dirty    "
          else do
            clean <- isTargetMarkedClean' depDir
            -- If we have already checked off this target as up to date, there is no need to check again
            if clean then return (True, "") `debug'` "+clean      "
            else do 
              let phonyTarget = phonyFile' depDir
              phonyTargetExists <- doesFileExist phonyTarget
              let existingTarget = if targetExists then target
                                   else if phonyTargetExists then phonyTarget else ""
              -- If neither a target or a phony target exists, then the target is obviously not up to date
              if null existingTarget then (returnFalse' depDir) `debug'` "-not built  "
              else return (False, existingTarget)
      where 
        debug' a b = debug'' level target a b
        isTargetMarkedClean' :: FilePath -> IO Bool 
        isTargetMarkedClean' metaDepsDir = doesFileExist =<< cleanFile' metaDepsDir
        isTargetMarkedDirty' :: FilePath -> IO Bool 
        isTargetMarkedDirty' metaDepsDir = doesFileExist =<< dirtyFile' metaDepsDir
        returnFalse' metaDepsDir = markTargetDirty' metaDepsDir >> return (False, "")

    -- Secondary up to date checks if the first checks fail to be conclusive. This function is
    -- meant to be called when a do file is not known
    upToDate2 :: Int -> FilePath -> FilePath -> IO Bool
    upToDate2 level target depDir = do
      doFile <- findDoFile target
      -- If no do file is found, but the meta dir exists, than this file used to be buildable, but is
      -- now a newly marked source file. So remove the meta dir and return true. There is no need to
      -- mark the file clean because the meta dir is removed.
      if isNothing doFile then (removeDirectoryRecursive depDir >> return True) `debug'` "+new source "
      else upToDate2' level target depDir (fromJust doFile)
      where 
        debug' a b = debug'' level target a b

    -- Secondary up to date checks if the first checks fail to be conclusive. This function is
    -- meant to be called when a do file is already known and is passed in.
    upToDate2' :: Int -> FilePath -> FilePath -> FilePath -> IO Bool
    upToDate2' level target depDir absDoFile = do
      newDo <- newDoFile depDir absDoFile
      -- If the target exists but a new do file was found for it then we need to rebuilt it, so
      -- it is not up to date.
      if newDo then (returnFalse depDir) `debug'` "-new .do   "
      else do
        let doFileDir = takeDirectory absDoFile
        -- If all of the dependencies are up to date then this target is also up to date, so mark it
        -- as such and return true. Else, return false.
        depsClean <- depsUpToDate level target depDir doFileDir 
        if depsClean then (returnTrue depDir) `debug'` "+deps clean "
        else (returnFalse depDir) -- `debug'` "-deps dirty "
      where 
        debug' a b = debug'' level target a b
        -- Does the target have a new do file from the last time it was built?
        newDoFile :: FilePath -> FilePath -> IO Bool
        newDoFile metaDepsDir doFile = 
          -- We shouldn't expect a do file to build another do file by default, so skip this check
          -- otherwise we end up with uncorrect behavior
          if (takeExtension target) == ".do" then return False
          else do
            maybe (return True) (pathsNotEqual doFile) =<< getCachedDoFile' metaDepsDir
          where pathsNotEqual path1 path2 = if path1 /= path2 then return True else return False

    -- Are a target's redo-create or redo-always or redo-ifchange dependencies up to date? 
    -- If so return, true, otherwise return false. Note that this function recurses on a target's
    -- dependencies to make sure the dependencies are up to date.
    depsUpToDate :: Int -> FilePath -> FilePath -> FilePath ->  IO Bool
    depsUpToDate level target metaDepsDir doFileDir = do
      depHashFiles <- getDirectoryContents metaDepsDir
      if anyAlwaysDeps depHashFiles then return False `debug` "-dep always "
      else do 
        -- redo-ifcreate - if one of those files was created, we need to return False immediately
        depCreated' <- mapOr (depCreated . unEscapeIfCreatePath) (ifCreateDeps depHashFiles)
        if depCreated' then return False `debug` "-dep created"
        -- redo-ifchange - check these files hashes against those stored to determine if they are up to date
        --                 then recursively check their dependencies to see if they are up to date
        else mapAnd (ifChangeDepsUpToDate metaDepsDir doFileDir) (ifChangeDeps depHashFiles)
      where 
        debug' a b = debug'' level target a b
        -- Function which basically does "and `liftM` mapM" but has the optimization of not continuing evaluation
        -- if a "False" is found. This helps prevent infinite loops if dependencies are circular.
        mapAnd :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
        mapAnd _ [] = return True
        mapAnd func (x:xs) = do boolean <- func x
                                if boolean then mapAnd func xs
                                -- Optimization: cut the evaluation short if a single False is found
                                else return False
        -- Function which basically does "or `liftM` mapM" but has the optimization of not continuing evaluation
        -- if a "True" is found.
        mapOr :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
        mapOr _ [] = return False
        mapOr func (x:xs) = do boolean <- func x
                               -- Optimization: cut the evaluation short if a single True is found
                               if boolean then return True
                               else mapOr func xs
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
        ifChangeDepsUpToDate :: FilePath -> FilePath -> FilePath -> IO Bool
        ifChangeDepsUpToDate depsDir doDir hashFile = do
          let dep = unEscapeIfChangePath hashFile
          let hashFullPath = depsDir </> hashFile
          let depFullPath = doDir </> dep
          -- Get the dependency to hash (phony or real). It it exists, calculate and 
          -- compare the hash. Otherwise, we know we are not up to date because the dep 
          -- is missing.
          depDir <- depFileDir depFullPath
          (ret, builtTarget) <- getBuiltTargetIfUpToDate (level+1) depFullPath depDir 
          -- Target is not built, so just exit out with the given return code
          if null builtTarget then return ret
          -- Target is built, let's hash against it
          else compareHash hashFullPath builtTarget depFullPath depDir
          --maybe (return False `debug'` "-dep missing") (compareHash depFullPath hashFullPath) =<< getBuiltTargetPath depFullPath
          where
            -- Check the hash of the dependency and compare it to the stored hash. This function provides recursion:
            compareHash :: FilePath -> FilePath -> FilePath -> FilePath -> IO Bool
            compareHash hashFullPath depToHash depFullPath depMetaDir = do
              oldHash <- BS.readFile hashFullPath 
              newHash <- computeHash depToHash
              -- If the dependency is not up-to-date, then return false
              -- If the dependency is up-to-date then recurse to see if it's dependencies are up-to-date
              if oldHash /= newHash then return False `debug'` "-dep changed"
              -- If the target exists, but has no do file to build it, then it is a source file, and is up to date, so return true
              -- Otherwise, we need to check if the dep itself is up to date, so recurse.
              else upToDate2 (level+1) depFullPath depMetaDir 

-- Missing do error function:
noDoFileError :: FilePath -> IO()
noDoFileError target = do putErrorStrLn $ "Error: No .do file found for target '" ++ target ++ "'"
                          exitFailure

-- Functions to escape and unescape dependencies of different types:
escapeIfChangePath :: FilePath -> FilePath 
escapeIfChangePath = escapeDependencyPath ifchange_dependency_prepend
unEscapeIfChangePath :: FilePath -> FilePath 
unEscapeIfChangePath = unEscapeDependencyPath ifchange_dependency_prepend

escapeIfCreatePath :: FilePath -> FilePath 
escapeIfCreatePath = escapeDependencyPath ifcreate_dependency_prepend
unEscapeIfCreatePath :: FilePath -> FilePath 
unEscapeIfCreatePath = unEscapeDependencyPath ifcreate_dependency_prepend

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
  performActionInDir parentRedoPath storeAlwaysDep $ parentRedoTarget

-- Store dependencies given a store action and a list of dependencies to store:
storeDependencies :: (FilePath -> FilePath -> IO ()) -> [FilePath] -> IO ()  
storeDependencies storeAction dependencies = do 
  parentRedoPath <- getEnv "REDO_PATH" -- directory where .do file was run from
  parentRedoTarget <- getEnv "REDO_TARGET"
  dependenciesRel2Parent <- makeRelativeToParent parentRedoPath dependencies 
  mapM_ (performActionInDir parentRedoPath (storeAction parentRedoTarget)) dependenciesRel2Parent
  where
    makeRelativeToParent :: FilePath -> [FilePath] -> IO ([FilePath])
    makeRelativeToParent parent targets = do
      currentDir <- getCurrentDirectory
      -- All dependencies for the parent target should be stored in a .redo file in the
      -- parent target .do file invocation location.
      -- Note: All target listed here are relative to the current directory in the .do script. This could
      -- be different than the REDO_PATH variable, which represents the directory where the .do was invoked 
      -- if 'cd' was used in the .do script.
      -- So, let's get a list of targets relative to the parent .do file invocation location, REDO_PATH
      return $ map (makeRelative parent . (currentDir </>)) targets

-- If the dependency exists then store its hash:
storeIfChangeDep :: FilePath -> FilePath -> IO ()
storeIfChangeDep target dep = maybe (return ()) storeHash =<< getBuiltTargetPath dep
  where storeHash depToHash = do theDepFile <- ifChangeDepFile target dep
                                 h <- computeHash $ depToHash
                                 writeDepFile (theDepFile) h

storeIfCreateDep :: FilePath -> FilePath -> IO ()
storeIfCreateDep target dep = createEmptyDepFile =<< ifCreateDepFile target dep

storeAlwaysDep :: FilePath -> IO ()
storeAlwaysDep target = createEmptyDepFile =<< alwaysDepFile target 

storePhonyTarget :: FilePath -> IO ()
storePhonyTarget target = createEmptyDepFile =<< phonyFile target 

-- Store a file to signify that this file has been checked, and is up
-- to date for this session.
markTargetClean :: FilePath -> IO ()
markTargetClean target = markTargetClean' =<< depFileDir target

markTargetClean' :: FilePath -> IO ()
markTargetClean' metaDepsDir = do
  removeSessionFiles metaDepsDir
  createEmptyDepFile =<< cleanFile' metaDepsDir

markTargetDirty' :: FilePath -> IO ()
markTargetDirty' metaDepsDir = do
  removeSessionFiles metaDepsDir
  createEmptyDepFile =<< dirtyFile' metaDepsDir

removeSessionFiles :: FilePath -> IO ()
removeSessionFiles metaDepsDir = safeRemoveGlob metaDepsDir ".cln.*.cln." >> safeRemoveGlob metaDepsDir ".drt.*.drt." 

removeLockFiles :: IO ()
removeLockFiles = do dir <- metaDir
                     safeRemoveGlob dir ".lck.*.lck."

safeRemoveGlob :: FilePath -> String -> IO ()
safeRemoveGlob directory globString = mapM_ safeRemove =<< globDir1 (compile globString) directory
  where safeRemove file = catch (removeFile file) (\(_ :: SomeException) -> return ())

-- Retrieve the cached do file path
isTargetMarkedClean :: FilePath -> IO (Bool)
isTargetMarkedClean target = doesFileExist =<< cleanFile target

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

-- Form the hash file path for a target's dependency given the current target and its dependency
depFile :: (FilePath -> FilePath) -> FilePath -> FilePath -> IO (FilePath)
depFile escapeFunc target dep = f =<< depFileDir target
  where f depDir = return $ depDir </> escapeFunc dep

-- Functions to get the dependency path for each file type
ifChangeDepFile :: FilePath -> FilePath -> IO (FilePath)
ifChangeDepFile = depFile escapeIfChangePath
ifCreateDepFile :: FilePath -> FilePath -> IO (FilePath)
ifCreateDepFile = depFile escapeIfCreatePath
alwaysDepFile :: FilePath -> IO (FilePath)
alwaysDepFile target = f =<< depFileDir target 
  where f depDir = return $ depDir </> "." ++ [always_dependency_prepend] ++ "redo-always" ++ [always_dependency_prepend] ++ "."
phonyFile :: FilePath -> IO (FilePath)
phonyFile target = do depDir <- depFileDir target 
                      return $ phonyFile' depDir
phonyFile' :: FilePath -> FilePath
phonyFile' metaDepsDir = metaDepsDir </> "." ++ "phony-target" ++ "."
-- todo instead of metaDepsDir, maybe we can simplify this and just call it the target's db entry or something..?
-- then we should get rid of any function referencing by target. Everything should be given a metadeps dir instead?
cleanFile :: FilePath -> IO (FilePath)
cleanFile target = do depDir <- depFileDir target
                      cleanFile' depDir
cleanFile' :: FilePath -> IO (FilePath)
cleanFile' metaDepsDir = f metaDepsDir =<< getEnv "REDO_SESSION"
  where f depDir session = return $ depDir </> ".cln." ++ session  ++ ".cln."

dirtyFile' :: FilePath -> IO (FilePath)
dirtyFile' metaDepsDir = f metaDepsDir =<< getEnv "REDO_SESSION"
  where f depDir session = return $ depDir </> ".drt." ++ session  ++ ".drt."
