{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module FilePathUtil(makeRelative', canonicalizePath', safeRemoveDirectoryRecursive, safeCreateDirectoryRecursive,
                    safeRemoveGlob, removeDotDirs, escapeFilePath, unescapeFilePath) where

import Control.Applicative ((<$>))
import Control.Exception (catch, SomeException(..), catchJust)
import Control.Monad (guard)
import System.FilePath (joinPath, splitDirectories, normalise, dropTrailingPathSeparator, (</>), isPathSeparator, pathSeparator, splitFileName)
import System.FilePath.Glob (globDir1, compile)
import System.Directory (removeFile, makeAbsolute, removeDirectoryRecursive, createDirectoryIfMissing)
import System.IO.Error (isDoesNotExistError)

---------------------------------------------------------------------
-- # Defines
---------------------------------------------------------------------
-- Some #defines used for creating escaped dependency filenames. We want to avoid /'s.
#define seperator_replacement '^'
#define seperator_replacement_escape '@'

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
escapeFilePath :: FilePath -> FilePath
escapeFilePath path = concatMap repl path'
  where --path' = sanitizeFilePath path
        path' = path
        repl seperator_replacement = seperator_replacement : [seperator_replacement_escape]
        repl c   = if isPathSeparator c then [seperator_replacement] else [c]

-- Reverses escapeFilePath
unescapeFilePath :: FilePath -> FilePath
unescapeFilePath name = unEscape name -- sanitizeFilePath $ unEscape name
  where 
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

-- Removes ".." and "." directories when possible:
removeDotDirs :: FilePath -> FilePath
removeDotDirs filePath = joinPath $ removeParents' [] (splitDirectories filePath)
  where removeParents' :: [String] -> [String] -> [String] 
        removeParents' [] [] = []
        removeParents' path [] = path
        removeParents' [] (h:hs) = removeParents' [h] hs
        removeParents' path (h : hs)
          | h == "." = removeParents' path hs
          | (h == "..") && (last path /= "") = removeParents' (init path) hs
          | otherwise = removeParents' (path ++ [h]) hs

-- Find the shared root between two paths:
findCommonRoot :: FilePath -> FilePath -> FilePath
findCommonRoot filePath1 filePath2 = joinPath $ findCommonRoot' (splitDirectories filePath1) (splitDirectories filePath2)
  where findCommonRoot' [] [] = []
        findCommonRoot' _ [] = []
        findCommonRoot' [] _ = []
        findCommonRoot' (h1:path1) (h2:path2) = if h1 == h2 then h1 : findCommonRoot' path1 path2
                                                else []

-- My version of makeRelative which actually works and inserts proper ".." where it can
makeRelative' :: FilePath -> FilePath -> FilePath
makeRelative' filePath1 filePath2 = if numParentDirs >= 0 then joinPath (replicate numParentDirs "..") </> path2NoRoot
                                    else filePath2
  where commonRoot = findCommonRoot filePath1 filePath2
        rootSize = length $ splitDirectories commonRoot
        path1Size = length $ splitDirectories filePath1
        numParentDirs = path1Size - rootSize
        path2NoRoot = joinPath $ drop rootSize $ splitDirectories filePath2

-- A faster version of canonicalizePath from System.Directory that doesn't care about resolving simlinks. This is
-- not a necessary feature for redo, and it just slows us down.
canonicalizePath' :: FilePath -> IO FilePath
canonicalizePath' path = removeDotDirs <$> makeAbsolute path

-- Remove files that match a globString, ie. "*.txt"
safeRemoveGlob :: FilePath -> String -> IO ()
safeRemoveGlob directory globString = mapM_ safeRemove =<< globDir1 (compile globString) directory
  where safeRemove file = catch (removeFile file) (\(_ :: SomeException) -> return ())

-- Remove a directory recursively without complaining if it exists or not:
safeRemoveDirectoryRecursive :: FilePath -> IO ()
safeRemoveDirectoryRecursive dir = catchJust (guard . isDoesNotExistError) (removeDirectoryRecursive dir) (\_ -> return())

-- Create a directory recursively withoutc complaining if it already exists:
safeCreateDirectoryRecursive :: FilePath -> IO ()
safeCreateDirectoryRecursive dir = catch (createDirectoryIfMissing True dir) (\(_ :: SomeException) -> return ())
