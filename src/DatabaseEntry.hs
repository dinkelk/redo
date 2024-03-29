module DatabaseEntry(Entry(..), doesEntryExist, removeEntry, createEntry, writeEntry, appendEntry, readEntry1, readEntry) where

import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>))

import FilePathUtil

---------------------------------------------------------------------
-- Type Definitions:
---------------------------------------------------------------------
-- This file stores name value pairs as directories. The entry is the
-- name and is stored as a directory. A list of values can be stored
-- inside of an entry, represented by nested directories.
newtype Entry = Entry { entryToFilePath :: FilePath } deriving (Eq, Show) -- The meta directory associated with a target

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
createEntry entry = safeCreateDirectoryRecursive (entryToFilePath entry)

-- Removal of an database entry:
removeEntry :: Entry -> IO ()
removeEntry entry = safeRemoveDirectoryRecursive (entryToFilePath entry)

-- Write to a newly created entry or overwrite the previous entry:
writeEntry :: Entry -> String -> IO ()
writeEntry entry contents = do
  safeRemoveDirectoryRecursive entry'
  safeCreateDirectoryRecursive dir
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
  contents <- listDirectory entry'
  return $ head contents
  where entry' = entryToFilePath entry

-- Read all of the values from an entry
readEntry :: Entry -> IO [String]
readEntry entry = listDirectory entry'
  where entry' = entryToFilePath entry
