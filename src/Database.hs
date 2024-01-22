{-# LANGUAGE ScopedTypeVariables #-}

module Database (clearRedoTempDirectory, initializeTargetDatabase, hasAlwaysDep, getIfCreateDeps,
                 getIfChangeDeps, storePhonyTarget, markClean, storeIfCreateDep, markDirty, storeStamp,
                 doesDatabaseExist, storeIfChangeDep, storeAlwaysDep, getBuiltTargetPath, isDirty,
                 initializeSourceDatabase, isClean, getDoFile, getStamp, isSource, getKey, getTempKey,
                 TempKey(..), Key(..), initializeSession, getTargetLockFile, getJobServerPipe,
                 getStdoutFile, getTempFile, markBuilt, isBuilt, markErrored, isErrored) where

import Control.Exception (catch, SomeException(..))
import qualified Data.ByteString.Char8 as BS
import Crypto.Hash.MD5 (hash)
import Data.Hex (hex)
import Data.Bool (bool)
import System.Directory (getAppUserDataDirectory, getTemporaryDirectory, doesDirectoryExist)
import System.FileLock (SharedExclusive(..), withFileLock)
import System.FilePath ((</>))
import System.Exit (exitFailure)
import System.Environment (getEnv, setEnv)
import System.Random (randomRIO)

import DatabaseEntry
import PrettyPrint
import FilePathUtil
import Types

---------------------------------------------------------------------
-- Type Definitions:
---------------------------------------------------------------------
newtype Key = Key { keyToFilePath :: FilePath } deriving (Show, Eq) -- The database key for a target
newtype TempKey = TempKey { tempKeyToFilePath :: FilePath } deriving (Show, Eq) -- The database key for a target

---------------------------------------------------------------------
-- Root database directory getters:
---------------------------------------------------------------------
-- Directory for storing and fetching data on dependencies of redo targets.
redoMetaDirectory :: IO FilePath
redoMetaDirectory = getAppUserDataDirectory "redo"

getUsername :: IO String
getUsername = catch (getEnv "USERNAME")
                (\(_ :: SomeException) -> catch (getEnv "USER")
                  (\(_ :: SomeException) -> getEnv "REDO_SESSION" )
                )

-- Directory for storing temporary data:
redoTempDirectory :: IO FilePath
redoTempDirectory = do base <- getTemporaryDirectory
                       session <- getEnv "REDO_SESSION"
                       user <- getUsername
                       return $ base </> "redo-" ++ user </> session

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
  root <- redoTempDirectory
  return $ root </> "cache"

-- Directory for storing target file locks to syncronize parallel builds of targets
redoTargetLockFileDirectory :: IO FilePath
redoTargetLockFileDirectory = do
  root <- redoTempDirectory
  return $ root </> "target_locks"

-- Directory for storing database file locks to syncronize parallel database access
redoDatabaseLockFileDirectory :: IO FilePath
redoDatabaseLockFileDirectory = do
  root <- redoTempDirectory
  return $ root </> "db_locks"

-- Directory for storing temporary target files to automically building redo files
redoTempTargetDirectory :: IO FilePath
redoTempTargetDirectory = do
  root <- redoTempDirectory
  return $ root </> "temp"

-- Directory for storing temporary target files to automically building redo files
redoStdoutTargetDirectory :: IO FilePath
redoStdoutTargetDirectory = do
  root <- redoTempDirectory
  return $ root </> "stdout"

---------------------------------------------------------------------
-- Functions for getting database directories for a target:
---------------------------------------------------------------------
-- Get the lock file prefix for a target:
getTargetLockFileBase :: TempKey -> IO FilePath
getTargetLockFileBase key = do
  lockFileDir <- redoTargetLockFileDirectory
  return $ lockFileDir </> tempKeyToFilePath key

getDatabaseLockFileBase :: TempKey -> IO FilePath
getDatabaseLockFileBase key = do
  lockFileDir <- redoDatabaseLockFileDirectory
  return $ lockFileDir </> tempKeyToFilePath key

-- Get the temp file prefix for a target:
getTempTargetDatabase :: TempKey -> IO FilePath
getTempTargetDatabase key = do
  tempFileDir <- redoTempTargetDirectory
  return $ tempFileDir </> tempKeyToFilePath key

-- Get the stdout file prefix for a target:
getStdoutTargetDatabase :: TempKey -> IO FilePath
getStdoutTargetDatabase key = do
  stdoutFileDir <- redoStdoutTargetDirectory
  return $ stdoutFileDir </> tempKeyToFilePath key

-- Get the database directory for a target's stamp:
getStampDatabase :: Key -> IO FilePath
getStampDatabase key = do
  stampDir <- redoStampDirectory
  return $ stampDir </> keyToFilePath key

-- Get the database directory for a target:
getDatabase :: Key -> IO FilePath
getDatabase key = do
  databaseDirectory <- redoDatabaseDirectory
  return $ databaseDirectory </> keyToFilePath key

---------------------------------------------------------------------
-- Functions getting database entries:
---------------------------------------------------------------------
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

-- Get the database entry for a target's ifcreate dependencies:
getErroredEntry :: Key -> IO Entry
getErroredEntry = getDatabaseEntry "e"

-- Get the database entry for a target's always dependencies:
getAlwaysEntry :: Key -> IO Entry
getAlwaysEntry = getDatabaseEntry "a"

-- Get the database entry for a target's always dependencies:
getPhonyTargetEntry :: Key -> IO Entry
getPhonyTargetEntry = getDatabaseEntry "p"

-- Get the database entry for a target's do file name:
getDoFileEntry :: Key -> IO Entry
getDoFileEntry = getDatabaseEntry "d"

-- Get the database entry for a target's stamp
getStampEntry :: Key -> IO Entry
getStampEntry key = do
  stampDir <- getStampDatabase key
  return $ Entry $ stampDir </> "s"

-- Get the cache directory for a target:
getCacheDatabase :: TempKey -> IO FilePath
getCacheDatabase key = do
  cacheDir <- redoCacheDirectory
  return $ cacheDir </> tempKeyToFilePath key

-- Generic function for getting a cache entry with a certain name:
getCacheEntry :: FilePath -> TempKey -> IO Entry
getCacheEntry name key = do
  cacheDir <- getCacheDatabase key
  return $ Entry $ cacheDir ++ name

-- Get the entry for marking a target clean:
getBuiltEntry :: TempKey -> IO Entry
getBuiltEntry = getCacheEntry "b"

-- Get the entry for marking a target clean:
getCleanEntry :: TempKey -> IO Entry
getCleanEntry = getCacheEntry "c"

-- Get the entry for marking a target dirty:
getDirtyEntry :: TempKey -> IO Entry
getDirtyEntry = getCacheEntry "d"

-- Get the filename for a lockfile for a particular target:
getTargetLockFile :: Target -> IO FilePath
getTargetLockFile target = do
  lockFileDir <- getTargetLockFileBase key
  return $ lockFileDir ++ "l"
  where key = getTempKey target

-- Get the filename for a lockfile for a particular database entry:
getDatabaseLockFile :: Key -> IO FilePath
getDatabaseLockFile key = do
  lockFileDir <- getDatabaseLockFileBase tempKey
  return $ lockFileDir ++ "l"
  where tempKey = keyToTempKey key

getDatabaseLockFile' :: TempKey -> IO FilePath
getDatabaseLockFile' tempKey = do
  lockFileDir <- getDatabaseLockFileBase tempKey
  return $ lockFileDir ++ "l"

-- Get temporary target file for a particular target:
getTempFile :: Target -> IO FilePath
getTempFile target = do
  tempFileDir <- getTempTargetDatabase key
  return $ tempFileDir ++ "f"
  where key = getTempKey target

-- Get temporary target file for a particular target:
getStdoutFile :: Target -> IO FilePath
getStdoutFile target = do
  stdoutFileDir <- getStdoutTargetDatabase key
  return $ stdoutFileDir ++ "f"
  where key = getTempKey target

-- Get the file for the job server named pipe:
getJobServerPipe :: IO FilePath
getJobServerPipe = do
  lockFileDir <- redoTempDirectory
  return $ lockFileDir </> "tokenpipe"

---------------------------------------------------------------------
-- Private (non thread safe) functions creating and removing target databases:
---------------------------------------------------------------------
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

---------------------------------------------------------------------
-- Private (non thread safe) functions modifying target databases:
---------------------------------------------------------------------
-- Store ifchange dependencies for a target:
storeIfChangeDep' :: Key -> Target -> IO ()
storeIfChangeDep' key dep = do
  ifChangeEntry <- getIfChangeEntry key
  appendEntry ifChangeEntry (escapeFilePath $ unTarget dep)

-- Cache the do file name in the target's database:
storeDoFile' :: Key -> DoFile -> IO ()
storeDoFile' key doFile = do
  doFileDir <- getDoFileEntry key
  writeEntry doFileDir (escapeFilePath $ unDoFile doFile)

-- Store a stamp for the target in the database:
storeStamp' :: Key -> Stamp -> IO ()
storeStamp' key stamp = do
  stampDir <- getStampEntry key
  writeEntry stampDir (show ratio)
  where ratio = toRational $ unStamp stamp

-- Mark a target as a source file in the cache:
markSource' :: Key -> IO ()
markSource' key = createEntry =<< getSourceEntry key

---------------------------------------------------------------------
-- Private functions for providing safe concurrent database access:
---------------------------------------------------------------------
withDatabaseLock :: Key -> IO a -> IO a
withDatabaseLock key action = do
  dbLockFile <- getDatabaseLockFile key
  withFileLock dbLockFile Exclusive f
  where f _ = action

withDatabaseLock' :: TempKey -> IO a -> IO a
withDatabaseLock' tempKey action = do
  dbLockFile <- getDatabaseLockFile' tempKey
  withFileLock dbLockFile Exclusive f
  where f _ = action

---------------------------------------------------------------------
-- Public functions getting keys for targets:
---------------------------------------------------------------------
-- Create a hash string for a target:
-- Note: For best results make sure you pass a canonicalized target
hashString :: Target -> FilePath
hashString target = hex $ BS.unpack $ hash $ BS.pack $ unTarget target

-- Specialized pathify function for making key paths
keyPathify :: FilePath -> FilePath
keyPathify = pathify 3

-- Get the database for a given target:
getKey :: Target -> Key
getKey target = Key $ keyPathify $ hashString target

-- The cache key is simpler, since it is temporary we don't
-- need to make so many directories
getTempKey :: Target -> TempKey
getTempKey target = TempKey $ hashString target

-- Conversion function from key to tempKey (unused, but left commented jic)
-- tempKeyToKey :: TempKey -> Key
-- tempKeyToKey tempKey = Key $ keyPathify $ tempKeyToFilePath tempKey

-- Conversion function from tempKey to key
keyToTempKey :: Key -> TempKey
keyToTempKey key = TempKey $ unpathify $ keyToFilePath key

---------------------------------------------------------------------
-- Public functions initializing a redo session:
---------------------------------------------------------------------

initializeSession :: IO ()
initializeSession = do
  sessionNumber <- randomRIO (0, 1000000::Int)
  setEnv "REDO_SESSION" (show sessionNumber)
  createRedoTempDirectory
  return ()

---------------------------------------------------------------------
-- Public functions creating and clearing the cache
---------------------------------------------------------------------
-- Clear entire temporary directory:
clearRedoTempDirectory :: IO ()
clearRedoTempDirectory = safeRemoveDirectoryRecursive =<< redoTempDirectory

-- Get the cache directory for a target:
createRedoTempDirectory :: IO ()
createRedoTempDirectory = do
  safeCreateDirectoryRecursive =<< redoCacheDirectory
  safeCreateDirectoryRecursive =<< redoDatabaseLockFileDirectory
  safeCreateDirectoryRecursive =<< redoTargetLockFileDirectory
  safeCreateDirectoryRecursive =<< redoTempTargetDirectory
  safeCreateDirectoryRecursive =<< redoStdoutTargetDirectory

---------------------------------------------------------------------
-- Public functions that need database locking:
---------------------------------------------------------------------
-- Create meta data folder for storing hashes and/or timestamps and return the folder name
-- We store a dependency for the target on the do file
-- Note: this function also blows out the old directory, which is good news because we don't want old
-- dependencies hanging around if we are rebuilding a file.
initializeTargetDatabase :: Key -> DoFile -> IO ()
initializeTargetDatabase key doFile = withDatabaseLock key func
  where func = do refreshDatabase key
                  -- Write out .do script as dependency:
                  storeIfChangeDep' key (Target $ unDoFile doFile)
                  -- Cache the do file:
                  storeDoFile' key doFile

initializeSourceDatabase :: Key -> Target -> IO ()
initializeSourceDatabase key target = withDatabaseLock key func
  where func = do refreshDatabase key
                  -- Write out the source file stamp:
                  stamp <- stampTarget target
                  storeStamp' key stamp
                  -- Mark this target as source:
                  markSource' key

-- Get the database directory for a target:
doesDatabaseExist :: Key -> IO Bool
doesDatabaseExist key = withDatabaseLock key func
  where func = doesDirectoryExist =<< getDatabase key

---------------------------------------------------------------------
-- Functions reading database values:
---------------------------------------------------------------------
-- Get the cached timestamp for when a target was last built. Return '.'
getStamp :: Key -> IO (Maybe Stamp)
getStamp key = withDatabaseLock key func
  where func = catch ( do
                 stampDir <- getStampEntry key
                 contents <- readEntry1 stampDir
                 return $ Just $ Stamp $ fromRational (read contents :: Rational))
               (\(_ :: SomeException) -> return Nothing)

-- Get the stored if create dependencies for a target:
getIfCreateDeps :: Key -> IO [Target]
getIfCreateDeps key = withDatabaseLock key func
  where func = do ifCreateEntry <- getIfCreateEntry key
                  getIfCreateDeps' ifCreateEntry
                  where getIfCreateDeps' entry =
                          catch (do
                            targets <- readEntry entry
                            return $ map convert targets)
                          (\(_ :: SomeException) -> return [])
                        convert = Target . unescapeFilePath

-- Get the stored if change dependencies for a target:
getIfChangeDeps :: Key -> IO [Target]
getIfChangeDeps key = withDatabaseLock key func
  where func = do ifChangeDir <- getIfChangeEntry key
                  getIfChangeDeps' ifChangeDir
                  where getIfChangeDeps' entry =
                          catch (do
                            targets <- readEntry entry
                            return $ map convert targets)
                          (\(_ :: SomeException) -> return [])
                        convert = Target . unescapeFilePath

-- Has the target been marked as errored in the database:
isErrored :: Key -> IO Bool
isErrored key = withDatabaseLock key func
  where func = doesEntryExist =<< getErroredEntry key

-- Has the target been marked clean in the cache?:
isBuilt :: TempKey -> IO Bool
isBuilt key = withDatabaseLock' key func
  where func = doesEntryExist =<< getBuiltEntry key

-- Has the target been marked clean in the cache?:
isClean :: TempKey -> IO Bool
isClean key = withDatabaseLock' key func
  where func = doesEntryExist =<< getCleanEntry key

-- Has the target been marked dirty in the cache?:
isDirty :: TempKey -> IO Bool
isDirty key = withDatabaseLock' key func
  where func = doesEntryExist =<< getDirtyEntry key

-- Retrieve the cached do file path stored in the database:
getDoFile :: Key -> IO (Maybe DoFile)
getDoFile key = withDatabaseLock key func
  where func = do doFileDir <- getDoFileEntry key
                  catch(readDoFile doFileDir) (\(_ :: SomeException) -> return Nothing)
                  where readDoFile dir = do doFile <- readEntry1 dir
                                            return $ Just $ DoFile $ unescapeFilePath doFile

-- Returns the path to the target, if it exists, otherwise it returns the path to the
-- phony target if it exists, else return Nothing
getBuiltTargetPath :: Key -> Target -> IO(Maybe Target)
getBuiltTargetPath key target = withDatabaseLock key func
  where func = do phonyEntry <- getPhonyTargetEntry key
                  returnTargetIfExists (returnPhonyIfExists (return Nothing) phonyEntry) target
                  where returnTargetIfExists failFunc file = bool failFunc (return $ Just file) =<< doesTargetExist file
                        returnPhonyIfExists failFunc entry = bool failFunc (return $ Just $ Target $ entryToFilePath entry) =<< doesEntryExist entry

---------------------------------------------------------------------
-- Functions writing database entries:
---------------------------------------------------------------------
storeIfChangeDep :: Key -> Target -> IO ()
storeIfChangeDep key dep = withDatabaseLock key (storeIfChangeDep' key dep)

-- Store the ifcreate dep only if the target doesn't exist right now
storeIfCreateDep :: Key -> Target -> IO ()
storeIfCreateDep key dep = withDatabaseLock key (bool storeIfCreateDep'
  (putErrorStrLn ("Error: Running redo-ifcreate on '" ++ unTarget dep ++ "' failed because it already exists.") >> exitFailure) =<< doesTargetExist dep)
  where storeIfCreateDep' :: IO ()
        storeIfCreateDep' = do
          ifCreateEntry <- getIfCreateEntry key
          appendEntry ifCreateEntry (escapeFilePath $ unTarget dep)

-- Store an "always dirty" dependency for a target:
storeAlwaysDep :: Key -> IO ()
storeAlwaysDep key = withDatabaseLock key func
  where func = createEntry =<< getAlwaysEntry key

-- Check to see if a target has an "always dirty" dependency:
hasAlwaysDep :: Key -> IO Bool
hasAlwaysDep key = withDatabaseLock key func
  where func = doesEntryExist =<< getAlwaysEntry key

-- Check to see if a target is a source file:
isSource :: Key -> IO Bool
isSource key = withDatabaseLock key func
  where func = doesEntryExist =<< getSourceEntry key

-- Store a phony target for the given target in the database:
storePhonyTarget :: Key -> IO ()
storePhonyTarget key = withDatabaseLock key func
  where func = do phonyTargetDir <- getPhonyTargetEntry key
                  writeEntry phonyTargetDir (escapeFilePath ".")

-- Mark a target as errored in the database:
markErrored :: Key -> IO ()
markErrored key = withDatabaseLock key func
  where func = createEntry =<< getErroredEntry key

-- Mark a target as built in the cache:
markBuilt :: TempKey -> IO ()
markBuilt key = withDatabaseLock' key func
  where func = createEntry =<< getBuiltEntry key

-- Mark a target as clean in the cache:
markClean :: TempKey -> IO ()
markClean key = withDatabaseLock' key func
  where func = createEntry =<< getCleanEntry key

-- Mark a target as dirty in the cache:
markDirty :: TempKey -> IO ()
markDirty key = withDatabaseLock' key func
  where func = createEntry =<< getDirtyEntry key

-- Store a stamp for the target in the database:
storeStamp :: Key -> Stamp -> IO ()
storeStamp key stamp = withDatabaseLock key (storeStamp' key stamp)
