{-# LANGUAGE ScopedTypeVariables #-}

module Helpers(performActionInDir, findDoFile, doesTargetExist, debug, makeRelative', canonicalizePath', 
               safeRemoveGlob, safeRemoveDirectoryRecursive, mapAnd, mapOr) where

import Control.Monad (guard)
import Control.Exception (catch, catchJust, SomeException(..))
import Control.Applicative ((<$>),(<*>))
import Control.Exception (catch, SomeException(..))
import Control.Monad (liftM, filterM)
import Data.Bool (bool)
import Data.Maybe (isNothing, listToMaybe)
import Debug.Trace (trace)
import System.FilePath (joinPath, splitDirectories, (</>), takeDirectory, isDrive, takeExtensions, dropExtensions, dropExtension, pathSeparator, splitFileName)
import System.FilePath.Glob (globDir1, compile)
import System.Directory (removeDirectoryRecursive, removeFile, setCurrentDirectory, doesFileExist, makeAbsolute, getCurrentDirectory, doesDirectoryExist)
import System.Exit (exitFailure)
import System.IO.Error (isDoesNotExistError)

import PrettyPrint

-- Debug helpers:
debug :: c -> String -> c
debug = flip trace
--debug a b = a

-- This applies a function to a target in the directory provided and then
-- returns the current directory to the starting directory:
performActionInDir :: FilePath -> (FilePath -> IO ()) -> FilePath -> IO ()
performActionInDir dir action target = do
  topDir <- getCurrentDirectory
  catch (setCurrentDirectory dir) (\(_ :: SomeException) -> do 
    putErrorStrLn $ "Error: No such directory " ++ topDir </> dir
    exitFailure)
  action target
  setCurrentDirectory topDir

-- Returns the absolute path to the do file given the absolute path to the target:
findDoFile :: FilePath -> IO (Maybe FilePath)
findDoFile absTarget = do 
  let (targetDir, targetName) = splitFileName absTarget
  let targetDo = removeDotDirs $ absTarget ++ ".do" 
  bool (defaultDoPath targetDir targetName) (return $ Just targetDo) =<< doesFileExist targetDo
  where
    -- Try to find matching .do file by checking directories upwards of "." until a suitable match is 
    -- found or "/" is reached.
    defaultDoPath :: FilePath -> FilePath -> IO (Maybe FilePath)
    defaultDoPath absPath' name = do
      let absPath = if last absPath' == pathSeparator then takeDirectory absPath' else absPath'
      doFile <- listToMaybe `liftM` filterM doesFileExist (candidates absPath name)
      if isNothing doFile && not (isDrive absPath) then defaultDoPath (takeDirectory absPath) name
      else return doFile
    -- List the possible default.do file candidates relative to the given path:
    candidates path name = map (path </>) (defaults name)
    defaults name = map (++ ".do") (getDefaultDo $ "default" ++ takeExtensions name)
    -- Form all possible matching default.do files in order of preference:
    getDefaultDo :: FilePath -> [FilePath]
    getDefaultDo filename = filename : if smallfilename == filename then [] else getDefaultDo $ dropFirstExtension filename
      where smallfilename = dropExtension filename
            basefilename = dropExtensions filename
            dropFirstExtension fname = basefilename ++ takeExtensions (drop 1 (takeExtensions fname))

-- Does the target file or directory exist on the filesystem?
doesTargetExist :: FilePath -> IO Bool
doesTargetExist target = (||) <$> doesFileExist target <*> doesDirectoryExist target

-- Removes ".." and "." directories when possible:
removeDotDirs :: FilePath -> FilePath
removeDotDirs filePath = joinPath $ removeParents' [] (splitDirectories filePath)
  where removeParents' :: [String] -> [String] -> [String] 
        removeParents' [] [] = []
        removeParents' path [] = path
        removeParents' [] (h:hs) = removeParents' [h] hs
        removeParents' path (h:hs) = if h == "." then removeParents' path hs
                                     else if (h == "..") && (last path /= "") then removeParents' (init path) hs
                                          else removeParents' (path ++ [h]) hs

-- Find the shared root between two paths:
findCommonRoot :: FilePath -> FilePath -> FilePath
findCommonRoot filePath1 filePath2 = joinPath $ findCommonRoot' (splitDirectories filePath1) (splitDirectories filePath2)
  where findCommonRoot' [] [] = []
        findCommonRoot' _ [] = []
        findCommonRoot' [] _ = []
        findCommonRoot' (h1:path1) (h2:path2) = if h1 == h2 then [h1] ++ findCommonRoot' path1 path2
                                                else []

-- My version of makeRelative which actually works and inserts proper ".." where it can
makeRelative' :: FilePath -> FilePath -> FilePath
makeRelative' filePath1 filePath2 = if numParentDirs >= 0 then (joinPath $ replicate numParentDirs "..") </> path2NoRoot
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
