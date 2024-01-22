module DatabaseEntry(Entry(..), doesEntryExist, removeEntry, createEntry, writeEntry, appendEntry, readEntry1, readEntry) where

import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>))

import FilePathUtil

-- leveldb stuff
import Database.LevelDB
import Data.Default
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import System.Directory (getAppUserDataDirectory)
import System.IO (stderr, hPutStrLn)

---------------------------------------------------------------------
-- Type Definitions:
---------------------------------------------------------------------
-- This file stores name value pairs as directories. The entry is the
-- name and is stored as a directory. A list of values can be stored
-- inside of an entry, represented by nested directories.
newtype Entry = Entry { entryToFilePath :: FilePath } deriving (Eq, Show) -- The meta directory associated with a target

---------------------------------------------------------------------
-- LevelDB stuff:
---------------------------------------------------------------------

-- Directory for storing and fetching data on dependencies of redo targets.
redoMetaDirectory :: IO FilePath
redoMetaDirectory = getAppUserDataDirectory "redo"

-- createLevelDatabase :: IO ()
-- createLevelDatabase = runResourceT $ do
--   redoDir <- liftIO redoMetaDirectory
--   _ <- open redoDir defaultOptions{ createIfMissing = True, cacheSize = 2048 }
--   return ()

withDB :: (DB -> IO a) -> IO a
withDB action = runResourceT $ do
  redoDir <- liftIO redoMetaDirectory
  db <- open redoDir defaultOptions{ createIfMissing = True }
  liftIO $ action db

readLevelDatabase :: String -> IO (Maybe String)
readLevelDatabase key = withDB func
  where func db = do value <- get db def (BS.pack key)
                     return $ BS.unpack `fmap` value

writeLevelDatabase :: String -> String -> IO ()
writeLevelDatabase key value = withDB func
  where func db = do put db def{sync = True} (BS.pack key) (BS.pack value)

deleteLevelDatabase :: String -> IO ()
deleteLevelDatabase key = withDB func
  where func db = do delete db def (BS.pack key)

createEntry' :: Entry -> IO ()
createEntry' entry = writeLevelDatabase (entryToFilePath entry) ""

removeEntry' :: Entry -> IO ()
removeEntry' entry = deleteLevelDatabase (entryToFilePath entry)

writeEntry' :: Entry -> String -> IO ()
writeEntry' entry contents = writeLevelDatabase (entryToFilePath entry) contents

readEntry1' :: Entry ->  IO String
readEntry1' entry = do
  val <- readLevelDatabase (entryToFilePath entry)
  case val of
    Nothing -> return ""
    Just x -> return x

---------------------------------------------------------------------
-- Existance functions:
---------------------------------------------------------------------
doesEntryExist :: Entry -> IO Bool
doesEntryExist entry = doesDirectoryExist $ entryToFilePath entry

---------------------------------------------------------------------
-- Functions writing meta files:
---------------------------------------------------------------------
-- Creation of an empty database entry:
createEntry :: Entry -> IO ()
createEntry entry = do
  safeCreateDirectoryRecursive (entryToFilePath entry)
  createEntry' entry

-- Removal of an database entry:
removeEntry :: Entry -> IO ()
removeEntry entry = do
  safeRemoveDirectoryRecursive (entryToFilePath entry)
  removeEntry' entry

-- Write to a newly created entry or overwrite the previous entry:
writeEntry :: Entry -> String -> IO ()
writeEntry entry contents = do
  safeRemoveDirectoryRecursive entry'
  safeCreateDirectoryRecursive dir
  writeEntry' entry contents
  where entry' = entryToFilePath entry
        dir = entry' </> contents

-- Write a new value to the entry:
appendEntry :: Entry -> String -> IO ()
appendEntry entry contents =
  safeCreateDirectoryRecursive dir
  where entry' = entryToFilePath entry
        dir = entry' </> contents

-- Read the first value of the entry
readEntry1 :: Entry -> IO String
readEntry1 entry = do
  str <- readEntry1' entry
  -- printStr' str
  contents <- listDirectory entry'
  return $ head contents
  where entry' = entryToFilePath entry
        printStr' str | null str = return ()
                      | otherwise = hPutStrLn stderr str

-- Read all of the values from an entry
readEntry :: Entry -> IO [String]
readEntry entry = listDirectory entry'
  where entry' = entryToFilePath entry
