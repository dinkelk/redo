{-# LANGUAGE ScopedTypeVariables #-}

module Database (clearRedoTempDirectory, initializeTargetDatabase, hasAlwaysDep, getIfCreateDeps, 
                 getIfChangeDeps, storePhonyTarget, markClean, storeIfCreateDep, markDirty, storeStamp, 
                 doesDatabaseExist, storeIfChangeDep, storeAlwaysDep, getBuiltTargetPath, isDirty, 
                 initializeSourceDatabase, isClean, getDoFile, getStamp, isSource, getKey, getTempKey, 
                 TempKey(..), Key(..), initializeSession, getLockFile, getJobServerPipe) where

import Control.Exception (catch, SomeException(..))
import qualified Data.ByteString.Char8 as BS
import Crypto.Hash.MD5 (hash) 
import Data.Hex (hex)
import Data.Bool (bool)
import System.Directory (getAppUserDataDirectory, getTemporaryDirectory, doesDirectoryExist)
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
-- Functions initializing the meta directory for a target
---------------------------------------------------------------------
initializeSession :: IO ()
initializeSession = do
  sessionNumber <- randomRIO (0, 1000000::Int)
  setEnv "REDO_SESSION" (show sessionNumber)
  createRedoTempDirectory

-- Directory for storing and fetching data on dpendencies of redo targets.
redoMetaDirectory :: IO FilePath
redoMetaDirectory = getAppUserDataDirectory "redo"

-- Directory for storing temporary data:
redoTempDirectory :: IO FilePath
redoTempDirectory = do base <- getTemporaryDirectory
                       session <- getEnv "REDO_SESSION"
                       return $ base </> "redo" </> session

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

-- Directory for storing file locks to syncronize parallel builds
redoLockFileDirectory :: IO FilePath
redoLockFileDirectory = do
  root <- redoTempDirectory 
  return $ root </> "locks"

---------------------------------------------------------------------
-- Functions creating and clearing the cache
---------------------------------------------------------------------
-- Clear entire temporary directory:
clearRedoTempDirectory :: IO ()
clearRedoTempDirectory = safeRemoveDirectoryRecursive =<< redoTempDirectory

-- Get the cache directory for a target:
createRedoTempDirectory :: IO ()
createRedoTempDirectory = do
  safeCreateDirectoryRecursive =<< redoCacheDirectory
  safeCreateDirectoryRecursive =<< redoLockFileDirectory

---------------------------------------------------------------------
-- Functions getting database keys for targets
---------------------------------------------------------------------
-- Get the database for a given target:
getKey :: Target -> Key
getKey target = Key $ pathify 3 $ hashString target
  where pathify _ "" = ""
        pathify n string = x </> pathify (n+n) xs
          where (x,xs) = splitAt n string

-- The cache key is simpler, since it is temporary we don't
-- need to make so many directories
getTempKey :: Target -> TempKey
getTempKey target = TempKey $ hashString target

-- Create a hash string for a target:
-- Note: For best results make sure you pass a canonicalized target
hashString :: Target -> FilePath
hashString target = hex $ BS.unpack $ hash $ BS.pack $ unTarget target

---------------------------------------------------------------------
-- Functions for setting up a database for a target:
---------------------------------------------------------------------
-- Get the lock file directory for a target:
getLockFileDatabase :: TempKey -> IO FilePath 
getLockFileDatabase key = do
  lockFileDir <- redoLockFileDirectory
  return $ lockFileDir </> tempKeyToFilePath key

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
initializeTargetDatabase :: Key -> DoFile -> IO ()
initializeTargetDatabase key doFile = do
  refreshDatabase key
  -- Write out .do script as dependency:
  storeIfChangeDep key (Target $ unDoFile doFile)
  -- Cache the do file:
  storeDoFile key doFile

initializeSourceDatabase :: Key -> Target -> IO ()
initializeSourceDatabase key target = do
  refreshDatabase key
  -- Write out the source file stamp:
  stamp <- stampTarget target
  storeStamp key stamp
  -- Mark this target as source:
  markSource key

-- Get the database directory for a target:
doesDatabaseExist :: Key -> IO Bool
doesDatabaseExist key = doesDirectoryExist =<< getDatabase key

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
getCleanEntry :: TempKey -> IO Entry
getCleanEntry = getCacheEntry "c"

-- Get the entry for marking a target dirty:
getDirtyEntry :: TempKey -> IO Entry
getDirtyEntry = getCacheEntry "d"

-- Get the filename for a lockfile for a particular target:
getLockFile :: Target -> IO FilePath
getLockFile target = do 
  lockFileDir <- getLockFileDatabase key
  return $ lockFileDir ++ "l"
  where key = getTempKey target

-- Get the file for the job server named pipe:
getJobServerPipe :: IO FilePath
getJobServerPipe = do 
  lockFileDir <- redoTempDirectory
  return $ lockFileDir </> "tokenpipe"

---------------------------------------------------------------------
-- Functions getting database values:
---------------------------------------------------------------------
-- Get the cached timestamp for when a target was last built. Return '.'
getStamp :: Key -> IO (Maybe Stamp)
getStamp key = catch (
  do stampDir <- getStampEntry key
     contents <- readEntry1 stampDir
     return $ Just $ Stamp contents)
  (\(_ :: SomeException) -> return Nothing)

-- Get the stored if create dependencies for a target:
getIfCreateDeps :: Key -> IO [Target]
getIfCreateDeps key = do
  ifCreateEntry <- getIfCreateEntry key
  getIfCreateDeps' ifCreateEntry
  where getIfCreateDeps' entry = 
          catch (do 
            targets <- readEntry entry
            return $ map convert targets)
          (\(_ :: SomeException) -> return [])
        convert = Target . unescapeFilePath

-- Get the stored if change dependencies for a target:
getIfChangeDeps :: Key -> IO [Target]
getIfChangeDeps key = do
  ifChangeDir <- getIfChangeEntry key
  getIfChangeDeps' ifChangeDir
  where getIfChangeDeps' entry =
          catch (do 
            targets <- readEntry entry
            return $ map convert targets)
          (\(_ :: SomeException) -> return [])
        convert = Target . unescapeFilePath

-- Has the target been marked clean in the cache?:
isClean :: TempKey -> IO Bool 
isClean key = doesEntryExist =<< getCleanEntry key

-- Has the target been marked dirty in the cache?:
isDirty :: TempKey -> IO Bool 
isDirty key = doesEntryExist =<< getDirtyEntry key

-- Retrieve the cached do file path stored in the database:
getDoFile :: Key -> IO (Maybe DoFile)
getDoFile key = do
  doFileDir <- getDoFileEntry key
  catch(readDoFile doFileDir) (\(_ :: SomeException) -> return Nothing)
  where readDoFile dir = do doFile <- readEntry1 dir
                            return $ Just $ DoFile $ unescapeFilePath doFile

-- Returns the path to the target, if it exists, otherwise it returns the path to the
-- phony target if it exists, else return Nothing
getBuiltTargetPath :: Key -> Target -> IO(Maybe Target)
getBuiltTargetPath key target = do
  phonyEntry <- getPhonyTargetEntry key
  returnTargetIfExists (returnPhonyIfExists (return Nothing) phonyEntry) target
  where returnTargetIfExists failFunc file = bool failFunc (return $ Just file) =<< doesTargetExist file
        returnPhonyIfExists failFunc entry = bool failFunc (return $ Just $ Target $ entryToFilePath entry) =<< doesEntryExist entry

---------------------------------------------------------------------
-- Functions writing database entries:
---------------------------------------------------------------------
-- Store ifchange dependencies for a target:
storeIfChangeDep :: Key -> Target -> IO () 
storeIfChangeDep key dep = do
  ifChangeEntry <- getIfChangeEntry key
  appendEntry ifChangeEntry (escapeFilePath $ unTarget dep)

-- Store the ifcreate dep only if the target doesn't exist right now
storeIfCreateDep :: Key -> Target -> IO ()
storeIfCreateDep key dep = bool storeIfCreateDep'
  (putErrorStrLn ("Error: Running redo-ifcreate on '" ++ unTarget dep ++ "' failed because it already exists.") >> exitFailure) =<< doesTargetExist dep
  where storeIfCreateDep' :: IO () 
        storeIfCreateDep' = do
          ifCreateEntry <- getIfCreateEntry key
          appendEntry ifCreateEntry (escapeFilePath $ unTarget dep)

-- Store an "always dirty" dependency for a target:
storeAlwaysDep :: Key -> IO () 
storeAlwaysDep key = createEntry =<< getAlwaysEntry key

-- Check to see if a target has an "always dirty" dependency:
hasAlwaysDep :: Key -> IO Bool
hasAlwaysDep key = doesEntryExist =<< getAlwaysEntry key

-- Mark a target as a source file in the cache:
markSource :: Key -> IO () 
markSource key = createEntry =<< getSourceEntry key

-- Check to see if a target is a source file:
isSource :: Key -> IO Bool
isSource key = doesEntryExist =<< getSourceEntry key

-- Store a phony target for the given target in the database:
storePhonyTarget :: Key -> IO () 
storePhonyTarget key = do
  phonyTargetDir <- getPhonyTargetEntry key
  writeEntry phonyTargetDir (escapeFilePath ".")

-- Mark a target as clean in the cache:
markClean :: TempKey -> IO ()
markClean key = createEntry =<< getCleanEntry key

-- Mark a target as dirty in the cache:
markDirty :: TempKey -> IO ()
markDirty key = createEntry =<< getDirtyEntry key

-- Cache the do file name in the target's database:
storeDoFile :: Key -> DoFile -> IO ()
storeDoFile key doFile = do
  doFileDir <- getDoFileEntry key
  writeEntry doFileDir (escapeFilePath $ unDoFile doFile)

-- Store a stamp for the target in the database:
storeStamp :: Key -> Stamp -> IO ()
storeStamp key stamp = do
  stampDir <- getStampEntry key
  writeEntry stampDir (unStamp stamp)
