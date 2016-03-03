{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DatabaseEntry(Entry(..), doesEntryExist, createEntry, writeEntry, appendEntry, readEntry1, readEntry,
                     appendFileEntry, readFileEntry) where

import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))

import FilePathUtil
import PrettyPrint

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
  contents <- getDirectoryContents entry'
  return $ contents !! 2
  where entry' = entryToFilePath entry

-- Read all of the values from an entry
readEntry :: Entry -> IO [String]
readEntry entry = do
  contents <- getDirectoryContents entry'
  return $ drop 2 contents
  where entry' = entryToFilePath entry

-- appendFileEntry :: Entry -> String -> String -> IO ()
-- appendFileEntry entry name contents = do
--   safeCreateDirectoryRecursive entry'
--   writeFile dir contents
--   where entry' = entryToFilePath entry
--         dir = entry' </> name
-- 
-- -- Read all of the values from an entry
-- readFileEntry :: Entry -> IO [(String, String)]
-- readFileEntry entry = do
--   names' <- getDirectoryContents entry'
--   let names = drop 2 names'
--   mapM extract names
--   where entry' = entryToFilePath entry
--         extract file = do contents <- readFile $ entry' </> file
--                           return $ (file, contents)


appendFileEntry :: Entry -> String -> String -> IO ()
appendFileEntry entry name contents = do
  safeCreateDirectoryRecursive dir
  where entry' = entryToFilePath entry
        dir = entry' </> name </> escapeFilePath contents

-- Read all of the values from an entry
readFileEntry :: Entry -> IO [(String, String)]
readFileEntry entry = do
  names' <- getDirectoryContents entry'
  let names = drop 2 names'
  mapM extract names
  where entry' = entryToFilePath entry
        extract file = do contents <- getDirectoryContents $ entry' </> file
                          return $ (file, unescapeFilePath (contents !! 2))
