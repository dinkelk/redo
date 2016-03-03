{-# LANGUAGE ScopedTypeVariables #-}

module Database (getDatabase, clearCache, clearLockFiles, redoMetaDirectory, initializeTargetDatabase, 
                 hasAlwaysDep, getIfCreateDeps, getIfChangeDeps, storePhonyTarget, createLockFile, 
                 markClean, storeIfCreateDep, markDirty, storeStamp, doesDatabaseExist, storeIfChangeDep, 
                 storeAlwaysDep, getBuiltTargetPath, isDirty, initializeSourceDatabase, isClean, 
                 getDoFile, getStamp, isSource, getKey, isTargetSource, LockFile(..), Key(..)) where

import Control.Exception (catch, SomeException(..))
import qualified Data.ByteString.Char8 as BS
import Crypto.Hash.MD5 (hash) 
import Data.Hex (hex)
import Data.Bool (bool)
import System.Directory (getAppUserDataDirectory, doesDirectoryExist)
import System.FilePath ((</>))
import System.Exit (exitFailure)

import DatabaseEntry
import PrettyPrint
import Helpers
import Types

---------------------------------------------------------------------
-- Type Definitions:
---------------------------------------------------------------------
newtype Key = Key { keyToFilePath :: FilePath } deriving (Eq) -- The database key for a target
newtype LockFile = LockFile { lockFileToFilePath :: FilePath } deriving (Eq) -- A lock file for synchronizing access to meta directories

---------------------------------------------------------------------
-- Functions initializing the meta directory for a target
---------------------------------------------------------------------
-- Directory for storing and fetching data on dpendencies of redo targets.
redoMetaDirectory :: IO FilePath
redoMetaDirectory = getAppUserDataDirectory "redo"

-- Directory for storing dependency database entries for redo targets and sources
redoDatabaseDirectory :: IO FilePath
redoDatabaseDirectory = do
  root <- redoMetaDirectory
  return $ root </> "database"

-- Directory for storing stamps of redo targets and sources. We need this to 
-- persist failed builds, which is why it is not integrated into the database
-- directory. The database directory is reset at the beginning of a rebuild 
-- for a target.
redoStampDirectory :: IO FilePath
redoStampDirectory = do
  root <- redoMetaDirectory
  return $ root </> "stamps"

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
  stamp <- stampTarget target
  storeStamp key stamp
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
getPhonyTargetEntry :: Key -> IO Entry 
getPhonyTargetEntry = getDatabaseEntry "p"

-- Get the database entry for a target's do file name:
getDoFileEntry :: Key -> IO Entry
getDoFileEntry = getDatabaseEntry "d"

-- Get the database entry for a target's file name:
getTargetEntry :: Key -> IO Entry
getTargetEntry = getDatabaseEntry "t"

-- Get the database directory for a target's stamp:
getStampDatabase :: Key -> IO FilePath
getStampDatabase key = do
  stampDir <- redoStampDirectory 
  return $ stampDir </> keyToFilePath key 

-- Get the database entry for a target's stamp
getStampEntry :: Key -> IO Entry
getStampEntry key = do
  stampDir <- getStampDatabase key
  return $ Entry $ stampDir </> "s"

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
  appendEntry ifChangeEntry (escapeFilePath $ unTarget dep)

-- Store the ifcreate dep only if the target doesn't exist right now
storeIfCreateDep :: Key -> Target -> IO ()
storeIfCreateDep key dep = bool (storeIfCreateDep' key dep) 
  (putErrorStrLn ("Error: Running redo-ifcreate on '" ++ unTarget dep ++ "' failed because it already exists.") >> exitFailure) =<< doesTargetExist dep

storeIfCreateDep' :: Key -> Target -> IO () 
storeIfCreateDep' key target = do
  ifCreateDir <- getIfCreateEntry key
  appendEntry ifCreateDir (escapeFilePath $ unTarget target)

storeAlwaysDep :: Key -> IO () 
storeAlwaysDep key = createEntry =<< getAlwaysEntry key

hasAlwaysDep :: Key -> IO Bool
hasAlwaysDep key = doesEntryExist =<< getAlwaysEntry key

markSource :: Key -> IO () 
markSource key = createEntry =<< getSourceEntry key

isSource :: Key -> IO Bool
isSource key = doesEntryExist =<< getSourceEntry key

getIfCreateDeps :: Key -> IO [Target]
getIfCreateDeps key = do
  ifCreateEntry <- getIfCreateEntry key
  getIfCreateDeps' ifCreateEntry
  where 
    getIfCreateDeps' entry = 
      catch (do files <- readEntry entry
                return $ map (Target. (unescapeFilePath)) files)
            (\(_ :: SomeException) -> return [])

getIfChangeDeps :: Key -> IO [Target]
getIfChangeDeps key = do
  ifChangeDir <- getIfChangeEntry key
  getIfChangeDeps' ifChangeDir
  where 
    getIfChangeDeps' dir = 
      catch (do files <- readEntry dir
                return $ map (Target . (unescapeFilePath)) files)
            (\(_ :: SomeException) -> return [])

storePhonyTarget :: Key -> IO () 
storePhonyTarget key = do
  phonyTargetDir <- getPhonyTargetEntry key
  writeEntry phonyTargetDir (escapeFilePath $ ".")

markClean :: Key -> IO ()
markClean key = createEntry =<< getCleanEntry key

markDirty :: Key -> IO ()
markDirty key = createEntry =<< getDirtyEntry key

storeDoFile :: Key -> DoFile -> IO ()
storeDoFile key doFile = do
  doFileDir <- getDoFileEntry key
  writeEntry doFileDir (escapeFilePath $ unDoFile doFile)

storeTarget :: Key -> Target -> IO ()
storeTarget key target = do
  targetEntry <- getTargetEntry key
  writeEntry targetEntry (escapeFilePath $ unTarget target)

storeStamp :: Key -> Stamp -> IO ()
storeStamp key stamp = do
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
                            return $ Just $ (DoFile $ unescapeFilePath doFile)

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
isTargetSource :: Target -> IO Bool
isTargetSource target = bool (return False) (isTargetSource') =<< doesTargetExist target
  where isTargetSource' = do key <- getKey target
                             hasDB <- doesDatabaseExist key
                             markedSource <- isSource key
                             return $ not hasDB || markedSource
