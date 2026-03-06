{-# LANGUAGE ScopedTypeVariables, DerivingStrategies #-}

module Database (clearRedoTempDirectory, initializeTargetDatabase, hasAlwaysDep, getIfCreateDeps,
                 getIfChangeDeps, storePhonyTarget, markClean, storeIfCreateDep, markDirty, storeStamp,
                 doesDatabaseExist, storeIfChangeDep, storeAlwaysDep, getBuiltTargetPath, isDirty,
                 initializeSourceDatabase, isClean, getDoFile, getStamp, isSource, getKey, getTempKey,
                 TempKey(..), Key(..), initializeSession, getTargetLockFile, getJobServerPipe,
                 getStdoutFile, getTempFile, markBuilt, isBuilt, markErrored, isErrored,
                 getTargetInfo, TargetInfo(..)) where

import Control.Exception (catch, SomeException(..))
import qualified Data.ByteString.Char8 as BS
import Crypto.Hash (hashWith, MD5(..), Digest)
import qualified Data.ByteArray
import Data.Hex (hex)
import Data.Bool (bool)
import qualified Data.Text as T
import System.Directory (getAppUserDataDirectory, getTemporaryDirectory, doesDirectoryExist)
import System.FileLock (SharedExclusive(..), withFileLock)
import System.FilePath ((</>))
import System.Exit (exitFailure)
import System.Environment (getEnv, setEnv, lookupEnv)
import System.Posix.User (getEffectiveUserID)
import System.Random (randomRIO)

import DatabaseEntry
import PrettyPrint
import FilePathUtil
import SqliteDb
import Types

---------------------------------------------------------------------
-- Type Definitions:
---------------------------------------------------------------------
newtype Key = Key { keyToFilePath :: FilePath } deriving stock (Show, Eq) -- The database key for a target
newtype TempKey = TempKey { tempKeyToFilePath :: FilePath } deriving stock (Show, Eq) -- The database key for a target

---------------------------------------------------------------------
-- Root database directory getters:
---------------------------------------------------------------------
-- Directory for storing and fetching data on dependencies of redo targets.
redoMetaDirectory :: IO FilePath
redoMetaDirectory = getAppUserDataDirectory "redo"

getUsername :: IO String
getUsername = do
  mUsername <- lookupEnv "USERNAME"
  case mUsername of
    Just u | not (null u) -> return u
    _ -> do
      mUser <- lookupEnv "USER"
      case mUser of
        Just u | not (null u) -> return u
        _ -> do
          -- Fallback to UID instead of REDO_SESSION to avoid creating
          -- per-session temp directories that can collide or get cleaned
          -- up while another session is still running.
          uid <- getEffectiveUserID
          return $ show uid

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

-- Directory for storing target file locks to synchronize parallel builds of targets
-- Uses persistent directory (~/.redo/locks/) so locks work across separate redo sessions
redoTargetLockFileDirectory :: IO FilePath
redoTargetLockFileDirectory = do
  root <- redoMetaDirectory
  let dir = root </> "locks"
  safeCreateDirectoryRecursive dir
  return dir

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
hashString target = hex $ BS.unpack $ Data.ByteArray.convert (hashWith MD5 (BS.pack $ unTarget target) :: Digest MD5)

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
---------------------------------------------------------------------
-- Re-export TargetInfo from SqliteDb
---------------------------------------------------------------------
-- | Get all target info in one SQLite query (avoids multiple round-trips)
getTargetInfo :: Key -> IO (Maybe TargetInfo)
getTargetInfo key = withRedoDb $ \db ->
  dbGetTargetInfo db (keyText key)

---------------------------------------------------------------------
-- Persistent database functions (SQLite-backed):
---------------------------------------------------------------------

-- Helper to convert Key to Text for SQLite
keyText :: Key -> T.Text
keyText = T.pack . keyToFilePath

initializeTargetDatabase :: Key -> DoFile -> IO ()
initializeTargetDatabase key doFile = withRedoDb $ \db ->
  dbInitTarget db (keyText key) (T.pack $ unDoFile doFile)

initializeSourceDatabase :: Key -> Target -> IO ()
initializeSourceDatabase key target = do
  stamp <- stampTarget target
  let stampStr = show $ toRational $ unStamp stamp
  withRedoDb $ \db ->
    dbInitSource db (keyText key) (T.pack stampStr)

doesDatabaseExist :: Key -> IO Bool
doesDatabaseExist key = withRedoDb $ \db ->
  dbExists db (keyText key)

getStamp :: Key -> IO (Maybe Stamp)
getStamp key = withRedoDb $ \db -> do
  ms <- dbGetStamp db (keyText key)
  case ms of
    Just s  -> return $ Just $ Stamp $ fromRational (read (T.unpack s) :: Rational)
    Nothing -> return Nothing

getIfCreateDeps :: Key -> IO [Target]
getIfCreateDeps key = withRedoDb $ \db -> do
  deps <- dbGetIfCreateDeps db (keyText key)
  return $ map (Target . T.unpack) deps

getIfChangeDeps :: Key -> IO [Target]
getIfChangeDeps key = withRedoDb $ \db -> do
  deps <- dbGetIfChangeDeps db (keyText key)
  return $ map (Target . T.unpack) deps

isErrored :: Key -> IO Bool
isErrored key = withRedoDb $ \db ->
  dbIsErrored db (keyText key)

getDoFile :: Key -> IO (Maybe DoFile)
getDoFile key = withRedoDb $ \db -> do
  mf <- dbGetDoFile db (keyText key)
  return $ fmap (DoFile . T.unpack) mf

getBuiltTargetPath :: Key -> Target -> IO (Maybe Target)
getBuiltTargetPath key target = do
  exists <- doesTargetExist target
  if exists then return (Just target)
  else do
    -- For phony targets, we need a stable filesystem path for stamping.
    -- Create a phony marker directory under ~/.redo/phony/<key>/
    isPhony <- withRedoDb $ \db -> dbGetPhony db (keyText key)
    case isPhony of
      Just _  -> do dir <- getPhonyDir key
                    safeCreateDirectoryRecursive dir
                    return $ Just $ Target dir
      Nothing -> return Nothing

storePhonyTarget :: Key -> IO ()
storePhonyTarget key = do
  -- Store in SQLite
  withRedoDb $ \db -> dbStorePhony db (keyText key) (T.pack ".")
  -- Recreate the phony marker directory fresh (so mtime reflects this build)
  dir <- getPhonyDir key
  safeRemoveDirectoryRecursive dir
  safeCreateDirectoryRecursive dir

-- Get a stable filesystem path for a phony target
getPhonyDir :: Key -> IO FilePath
getPhonyDir key = do
  root <- redoMetaDirectory
  return $ root </> "phony" </> keyToFilePath key

storeIfChangeDep :: Key -> Target -> IO ()
storeIfChangeDep key dep = withRedoDb $ \db ->
  dbStoreIfChangeDep db (keyText key) (T.pack $ unTarget dep)

storeIfCreateDep :: Key -> Target -> IO ()
storeIfCreateDep key dep = do
  exists <- doesTargetExist dep
  if exists
    then putErrorStrLn ("Error: Running redo-ifcreate on '" ++ unTarget dep ++ "' failed because it already exists.") >> exitFailure
    else withRedoDb $ \db ->
      dbStoreIfCreateDep db (keyText key) (T.pack $ unTarget dep)

storeAlwaysDep :: Key -> IO ()
storeAlwaysDep key = withRedoDb $ \db ->
  dbStoreAlwaysDep db (keyText key)

hasAlwaysDep :: Key -> IO Bool
hasAlwaysDep key = withRedoDb $ \db ->
  dbHasAlwaysDep db (keyText key)

isSource :: Key -> IO Bool
isSource key = withRedoDb $ \db ->
  dbIsSource db (keyText key)

markErrored :: Key -> IO ()
markErrored key = withRedoDb $ \db ->
  dbMarkErrored db (keyText key)

storeStamp :: Key -> Stamp -> IO ()
storeStamp key stamp = withRedoDb $ \db ->
  dbStoreStamp db (keyText key) (T.pack $ show $ toRational $ unStamp stamp)

---------------------------------------------------------------------
-- Session-cache functions (filesystem-backed, in /tmp):
---------------------------------------------------------------------
isBuilt :: TempKey -> IO Bool
isBuilt key = doesEntryExist =<< getBuiltEntry key

isClean :: TempKey -> IO Bool
isClean key = doesEntryExist =<< getCleanEntry key

isDirty :: TempKey -> IO Bool
isDirty key = doesEntryExist =<< getDirtyEntry key

markBuilt :: TempKey -> IO ()
markBuilt key = createEntry =<< getBuiltEntry key

markClean :: TempKey -> IO ()
markClean key = createEntry =<< getCleanEntry key

markDirty :: TempKey -> IO ()
markDirty key = createEntry =<< getDirtyEntry key
