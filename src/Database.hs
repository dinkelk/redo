{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database (getDatabase, clearCache, clearLockFiles, redoMetaDirectory, initializeTargetDatabase, storeIfChangeDependencies, storeIfCreateDependencies, 
                     storeAlwaysDependency, hasAlwaysDep, getIfCreateDeps, getIfChangeDeps, getIfChangeDeps'', storePhonyTarget, createLockFile, markClean, unEscapeDependencyPath, removeDatabase,
                     markDirty, storeStamp, doesDatabaseExist,
                     getBuiltTargetPath, isDirty, initializeSourceDatabase,
                     isClean, getDoFile, getStamp, getIfChangeEntry, isSource,
                     isSourceFile, LockFile(..), MetaFile(..), Key(..), getKey) where

import Control.Applicative ((<$>))
import Control.Exception (catch, SomeException(..))
import qualified Data.ByteString.Char8 as BS
import Crypto.Hash.MD5 (hash) 
import Data.Hex (hex)
import Data.Bool (bool)
import System.Directory (getAppUserDataDirectory, getCurrentDirectory, doesDirectoryExist)
import System.FilePath (normalise, dropTrailingPathSeparator, makeRelative, splitFileName, (</>), isPathSeparator, pathSeparator)
import System.Environment (getEnv)
import System.Exit (exitFailure)

import DatabaseEntry
import PrettyPrint
import Helpers
import Types

---------------------------------------------------------------------
-- Type Definitions:
---------------------------------------------------------------------
newtype Key = Key { keyToFilePath :: FilePath } deriving (Eq) -- The database key for a target
newtype MetaFile = MetaFile { unMetaFile :: FilePath } deriving (Eq) -- A meta file stored within a meta directory
newtype LockFile = LockFile { lockFileToFilePath :: FilePath } deriving (Eq) -- A lock file for synchronizing access to meta directories

---------------------------------------------------------------------
-- # Defines
---------------------------------------------------------------------
-- Some #defines used for creating escaped dependency filenames. We want to avoid /'s.
#define seperator_replacement '^'
#define seperator_replacement_escape '@'

---------------------------------------------------------------------
-- Functions initializing the meta directory for a target
---------------------------------------------------------------------
-- Directory for storing and fetching data on dpendencies of redo targets.
redoMetaDirectory :: IO FilePath
redoMetaDirectory = getAppUserDataDirectory "redo"

-- Directory for storing database entries for redo targets and sources
redoDatabaseDirectory :: IO FilePath
redoDatabaseDirectory = do
  root <- redoMetaDirectory
  return $ root </> "database"

-- Directory for storing single redo session cached information. This speeds up
-- the upToDate function
redoCacheDirectory :: IO FilePath
redoCacheDirectory = do
  root <- redoMetaDirectory
  return $ root </> "cache"

-- Directory for storing file locks to syncronize parallel builds
redoLockFileDirectory :: IO FilePath
redoLockFileDirectory = do
  root <- redoMetaDirectory
  return $ root </> "locks"

---------------------------------------------------------------------
-- Functions clearing meta data information
---------------------------------------------------------------------
-- Clear the entire redo cache directory:
clearCache :: IO ()
clearCache = safeRemoveDirectoryRecursive =<< redoCacheDirectory

-- Clear the entire redo cache directory:
clearLockFiles :: IO ()
clearLockFiles = safeRemoveDirectoryRecursive =<< redoLockFileDirectory

---------------------------------------------------------------------
-- Functions getting database keys for targets
---------------------------------------------------------------------
-- Get the database for a given target:
getKey :: Target -> IO Key
getKey target = do
  hashedTarget <- hashString target
  return $ Key $ pathify hashedTarget
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

---------------------------------------------------------------------
-- Functions getting database entries:
---------------------------------------------------------------------
-- Get the database directory for a target:
getDatabase :: Key -> IO FilePath
getDatabase key = do
  databaseDirectory <- redoDatabaseDirectory
  return $ databaseDirectory </> keyToFilePath key

-- Create the database directory for a target:
createDatabase :: Key -> IO ()
createDatabase key = safeCreateDirectoryRecursive =<< getDatabase key

-- Remove a database directory:
removeDatabase :: Key -> IO ()
removeDatabase key = safeRemoveDirectoryRecursive =<< getDatabase key

-- Remove and then create a database directory:
refreshDatabase :: Key -> IO ()
refreshDatabase key = do
  removeDatabase key
  createDatabase key

-- Create meta data folder for storing hashes and/or timestamps and return the folder name
-- We store a dependency for the target on the do file
-- Note: this function also blows out the old directory, which is good news because we don't want old
-- dependencies hanging around if we are rebuilding a file.
initializeTargetDatabase :: Key -> Target -> DoFile -> IO ()
initializeTargetDatabase key target doFile = do
  refreshDatabase key
  -- Write out .do script as dependency:
  storeIfChangeDep key (Target $ unDoFile doFile)
  -- Write out target name:
  storeTarget key target
  -- Cache the do file:
  storeDoFile key doFile

initializeSourceDatabase :: Key -> Target -> IO ()
initializeSourceDatabase key target = do
  refreshDatabase key
  -- Write out the source file stamp:
  storeStamp key target
  -- Write out the source file name:
  storeTarget key target
  -- Mark this target as source:
  markSource key

-- Get the database directory for a target:
doesDatabaseExist :: Key -> IO Bool
doesDatabaseExist key = doesDirectoryExist =<< getDatabase key

-- Generic function for getting a database entry with a certain name:
getDatabaseEntry :: FilePath -> Key -> IO Entry
getDatabaseEntry name key = do
  dbDir <- getDatabase key
  return $ Entry $ dbDir </> name

-- Get the database entry for a target's ifchange dependencies:
getSourceEntry :: Key -> IO Entry
getSourceEntry = getDatabaseEntry "y"

-- Get the database entry for a target's ifchange dependencies:
getIfChangeEntry :: Key -> IO Entry
getIfChangeEntry = getDatabaseEntry "r"

-- Get the database entry for a target's ifcreate dependencies:
getIfCreateEntry :: Key -> IO Entry
getIfCreateEntry = getDatabaseEntry "c"

-- Get the database entry for a target's always dependencies:
getAlwaysEntry :: Key -> IO Entry 
getAlwaysEntry = getDatabaseEntry "a"

-- Get the database entry for a target's always dependencies:
getStampEntry :: Key -> IO Entry
getStampEntry = getDatabaseEntry "s"

-- Get the database entry for a target's always dependencies:
getPhonyTargetEntry :: Key -> IO Entry 
getPhonyTargetEntry = getDatabaseEntry "p"

-- Get the database entry for a target's do file name:
getDoFileEntry :: Key -> IO Entry
getDoFileEntry = getDatabaseEntry "d"

-- Get the database entry for a target's file name:
getTargetEntry :: Key -> IO Entry
getTargetEntry = getDatabaseEntry "t"

---------------------------------------------------------------------
-- Functions getting database entries:
---------------------------------------------------------------------
-- Get the cache directory for a target:
getCacheDatabase :: Key -> IO FilePath
getCacheDatabase key = do
  cacheDir <- redoCacheDirectory
  return $ cacheDir </> keyToFilePath key

-- Generic function for getting a cache entry with a certain name:
getCacheEntry :: FilePath -> Key -> IO Entry
getCacheEntry name key = do
  cacheDir <- getCacheDatabase key
  return $ Entry $ cacheDir </> name

-- Get the entry for marking a target clean:
getCleanEntry :: Key -> IO Entry
getCleanEntry = getCacheEntry "c"

-- Get the entry for marking a target dirty:
getDirtyEntry :: Key -> IO Entry
getDirtyEntry = getCacheEntry "d"

---------------------------------------------------------------------
-- Functions getting lock files:
---------------------------------------------------------------------
-- Get the lock file directory for a target:
getLockFileDatabase :: Key -> IO FilePath 
getLockFileDatabase key = do
  lockFileDir <- redoLockFileDirectory
  return $ lockFileDir </> keyToFilePath key

---------------------------------------------------------------------
-- Functions writing meta files:
---------------------------------------------------------------------
storeIfChangeDep :: Key -> Target -> IO () 
storeIfChangeDep key dep = do
  ifChangeEntry <- getIfChangeEntry key
  -- TODO... maybe just store the key instead?
  appendEntry ifChangeEntry (escapeDependencyPath '@' $ unTarget dep)

-- Store the ifcreate dep only if the target doesn't exist right now
storeIfCreateDep :: Key -> Target -> IO ()
storeIfCreateDep key dep = bool (storeIfCreateDep' key dep) 
  (putErrorStrLn ("Error: Running redo-ifcreate on '" ++ unTarget dep ++ "' failed because it already exists.") >> exitFailure) =<< doesTargetExist dep

storeIfCreateDep' :: Key -> Target -> IO () 
storeIfCreateDep' key target = do
  ifCreateDir <- getIfCreateEntry key
  appendEntry ifCreateDir (escapeDependencyPath '@' $ unTarget target)

storeAlwaysDep :: Key -> IO () 
storeAlwaysDep key = createEntry =<< getAlwaysEntry key

hasAlwaysDep :: Key -> IO Bool
hasAlwaysDep key = doesEntryExist =<< getAlwaysEntry key

markSource :: Key -> IO () 
markSource key = createEntry =<< getSourceEntry key

isSource :: Key -> IO Bool
isSource key = doesEntryExist =<< getSourceEntry key

getIfCreateDeps :: Key -> IO [MetaFile]
getIfCreateDeps key = do
  ifCreateEntry <- getIfCreateEntry key
  getIfCreateDeps' ifCreateEntry
  where 
    getIfCreateDeps' entry = 
      catch (do files <- readEntry entry
                return $ map (MetaFile . (unEscapeDependencyPath '@')) files)
            (\(_ :: SomeException) -> return [])

getIfChangeDeps :: Key -> IO [MetaFile]
getIfChangeDeps key = do
  ifChangeDir <- getIfChangeEntry key
  getIfChangeDeps' ifChangeDir
  where 
    getIfChangeDeps' dir = 
      catch (do files <- readEntry dir
                return $ map (MetaFile . (unEscapeDependencyPath '@')) files)
            (\(_ :: SomeException) -> return [])

getIfChangeDeps'' :: Key -> IO [MetaFile]
getIfChangeDeps'' key = do
  ifChangeDir <- getIfChangeEntry key
  getIfChangeDeps' ifChangeDir
  where 
    getIfChangeDeps' dir = 
      catch (do files <- readEntry dir
                return $ map MetaFile files)
            (\(_ :: SomeException) -> return [])

storePhonyTarget :: Key -> IO () 
storePhonyTarget key = do
  phonyTargetDir <- getPhonyTargetEntry key
  writeEntry phonyTargetDir (escapeDependencyPath '@' $ ".")

markClean :: Key -> IO ()
markClean key = createEntry =<< getCleanEntry key

markDirty :: Key -> IO ()
markDirty key = createEntry =<< getDirtyEntry key

storeDoFile :: Key -> DoFile -> IO ()
storeDoFile key doFile = do
  doFileDir <- getDoFileEntry key
  writeEntry doFileDir (escapeDependencyPath '@' $ unDoFile doFile)

storeTarget :: Key -> Target -> IO ()
storeTarget key target = do
  targetEntry <- getTargetEntry key
  writeEntry targetEntry (escapeDependencyPath '@' $ unTarget target)

storeStamp :: Key -> Target -> IO ()
storeStamp key target = do
  stamp <- stampTarget target
  stampDir <- getStampEntry key
  writeEntry stampDir (unStamp stamp)

-- Get the cached timestamp for when a target was last built. Return '.'
getStamp :: Key -> IO (Maybe Stamp)
getStamp key = catch (
  do stampDir <- getStampEntry key
     contents <- readEntry1 stampDir
     return $ Just $ Stamp $ contents)
  (\(_ :: SomeException) -> return Nothing)

-- Return the lock file name for a target:
-- TODO make this take a key
createLockFile :: Target -> IO LockFile
createLockFile target = do
  key <- getKey target
  createLockFile' key

createLockFile' :: Key -> IO LockFile
createLockFile' key = do 
  lockFileDir <- getLockFileDatabase key
  -- TODO cleanup:
  safeCreateDirectoryRecursive lockFileDir 
  return $ LockFile $ lockFileDir </> "l"

---------------------------------------------------------------------
-- Functions reading meta files:
---------------------------------------------------------------------
isClean :: Key -> IO Bool 
isClean key = doesEntryExist =<< getCleanEntry key

isDirty :: Key -> IO Bool 
isDirty key = doesEntryExist =<< getDirtyEntry key

-- Retrieve the cached do file path inside meta dir
getDoFile :: Key -> IO (Maybe DoFile)
getDoFile key = do
  doFileDir <- getDoFileEntry key
  catch(readDoFile doFileDir) (\(_ :: SomeException) -> return Nothing)
  where readDoFile dir = do doFile <- readEntry1 dir
                            return $ Just $ (DoFile $ unEscapeDependencyPath '@' doFile)

-- Returns the path to the target, if it exists, otherwise it returns the path to the
-- phony target if it exists, else return Nothing
getBuiltTargetPath :: Key -> Target -> IO(Maybe Target)
getBuiltTargetPath key target = do
  phonyEntry <- getPhonyTargetEntry key
  returnTargetIfExists (returnPhonyIfExists (return Nothing) (phonyEntry)) target
  where returnTargetIfExists failFunc file = bool failFunc (return $ Just file) =<< doesTargetExist file
        returnPhonyIfExists failFunc entry = bool failFunc (return $ Just $ Target $ entryToFilePath entry) =<< doesEntryExist entry

-- Does the target file or directory exist on the filesystem?
-- Checks if a target file is a buildable target, or if it is a source file
-- TODO this is probably broken
isSourceFile :: Target -> IO Bool
isSourceFile target = bool (return False) (not <$> hasDependencies target) =<< doesTargetExist target
  where
    -- Check's if a target has dependencies stored already
    hasDependencies :: Target -> IO Bool
    hasDependencies t = do
      key <- getKey t
      doesDirectoryExist =<< getDatabase key

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
        repl seperator_replacement = seperator_replacement : [seperator_replacement_escape]
        repl c   = if isPathSeparator c then [seperator_replacement] else [c]

-- Reverses escapeFilePath
-- TODO remove dep prepend and . from these functions
unEscapeDependencyPath :: Char -> FilePath -> FilePath
unEscapeDependencyPath dependency_prepend name = sanitizeFilePath path
  where 
    path = if take 2 name == ('.' : [dependency_prepend]) then unEscape $ (dropEnd 2 . drop 2) name else name
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

---------------------------------------------------------------------
-- Higher level functions
---------------------------------------------------------------------
-- Store dependencies for redo-ifchange:
storeIfChangeDependencies :: [Target] -> IO ()
storeIfChangeDependencies = storeDependencies storeIfChangeDep

-- Store dependencies for redo-ifcreate:
storeIfCreateDependencies :: [Target] -> IO ()
storeIfCreateDependencies = storeDependencies storeIfCreateDep

-- Store dependency for redo-always:
storeAlwaysDependency :: IO ()
storeAlwaysDependency = do 
  parentRedoTarget <- getEnv "REDO_TARGET"
  -- TODO: consider storing the key in the variable...
  key <- getKey $ Target parentRedoTarget
  storeAlwaysDep key

-- Store dependencies given a store action and a list of dependencies to store:
storeDependencies :: (Key -> Target -> IO ()) -> [Target] -> IO ()  
storeDependencies storeAction dependencies = do 
  parentRedoTarget <- getEnv "REDO_TARGET"
  parentRedoPath <- getEnv "REDO_PATH" -- directory where .do file was run from
  dependenciesRel2Parent <- makeRelativeToParent parentRedoPath dependencies 
  -- todo cleanup this geEnv
  key <- getKey $ Target parentRedoTarget
  mapM_ (storeAction key) dependenciesRel2Parent
  where
    makeRelativeToParent :: FilePath -> [Target] -> IO [Target]
    makeRelativeToParent parent targets = do
      currentDir <- getCurrentDirectory
      -- Note: All target listed here are relative to the current directory in the .do script. This could
      -- be different than the REDO_PATH variable, which represents the directory where the .do was invoked 
      -- if 'cd' was used in the .do script.
      -- So, let's get a list of targets relative to the parent .do file invocation location, REDO_PATH
      return $ map (Target . makeRelative parent . (currentDir </>) . unTarget) targets
