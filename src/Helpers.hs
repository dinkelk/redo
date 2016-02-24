{-# LANGUAGE ScopedTypeVariables #-}

module Helpers(debug, makeRelative', canonicalizePath', safeRemoveDirectoryRecursive,
               safeRemoveGlob, removeDotDirs, mapAnd, mapOr) where

import Control.Applicative ((<$>))
import Control.Exception (catch, SomeException(..), catchJust)
import Control.Monad (guard)
import Debug.Trace (trace)
import System.FilePath (joinPath, splitDirectories, (</>))
import System.FilePath.Glob (globDir1, compile)
import System.Directory (removeFile, makeAbsolute, removeDirectoryRecursive)
import System.IO.Error (isDoesNotExistError)

-- Debug helpers:
debug :: c -> String -> c
--debug = flip trace
debug a b = a

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
