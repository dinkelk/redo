{-# LANGUAGE ScopedTypeVariables #-}

module Helpers(performActionInDir, findDoFile, getTargetRel2Do, doesTargetExist) where

import Control.Applicative ((<$>),(<*>))
import Control.Exception (catch, SomeException(..))
import Control.Monad (liftM, filterM)
import Data.Bool (bool)
import Data.Maybe (isNothing, listToMaybe)
import System.FilePath (makeRelative, (</>), takeDirectory, isDrive, takeExtensions, dropExtensions, dropExtension, pathSeparator, splitFileName)
import System.Directory (setCurrentDirectory, doesFileExist, makeAbsolute, canonicalizePath, getCurrentDirectory, doesDirectoryExist)
import System.Exit (exitFailure)

import PrettyPrint

-- This applies a function to a target in the directory provided and then
-- returns the current directory to the starting directory:
performActionInDir :: FilePath -> (FilePath -> IO ()) -> FilePath -> IO ()
performActionInDir dir action target = do
  topDir <- getCurrentDirectory
  --redoTarget' <- lookupEnv "REDO_TARGET"
  --case (redoTarget') of 
  --  (Just redoTarget) -> hPutStrLn stderr $ "... redoing " ++ redoTarget ++ "* -> " ++ (target)
  --  (Nothing) -> hPutStrLn stderr $ "... redoing " ++ target ++ "  -> " ++ (target)
  catch (setCurrentDirectory dir) (\(_ :: SomeException) -> do 
    putErrorStrLn $ "Error: No such directory " ++ topDir </> dir
    exitFailure)
  action target
  setCurrentDirectory topDir

-- Todo: hash do files for each target on build, and use the hashes on ifchange
findDoFile :: FilePath -> IO (Maybe FilePath)
findDoFile target = bool (defaultDoPath targetDir) (return $ Just targetDo) =<< doesFileExist targetDo
  where
    (targetDir, targetName) = splitFileName target
    targetDo = target ++ ".do"
    -- Try to find matching .do file by checking directories upwards of "." until a suitable match is 
    -- found or "/" is reached.
    defaultDoPath :: FilePath -> IO (Maybe FilePath)
    defaultDoPath dir = do
      absPath' <- canonicalizePath dir
      let absPath = if last absPath' == pathSeparator then takeDirectory absPath' else absPath'
      doFile <- listToMaybe `liftM` filterM doesFileExist (candidates absPath)
      if isNothing doFile && not (isDrive absPath) then defaultDoPath $ takeDirectory absPath 
      else return doFile
    -- List the possible default.do file candidates relative to the given path:
    candidates path = map (path </>) defaults
    defaults = map (++ ".do") (getDefaultDo $ "default" ++ takeExtensions targetName)
    -- Form all possible matching default.do files in order of preference:
    getDefaultDo :: FilePath -> [FilePath]
    getDefaultDo filename = filename : if smallfilename == filename then [] else getDefaultDo $ dropFirstExtension filename
      where smallfilename = dropExtension filename
            basefilename = dropExtensions filename
            dropFirstExtension fname = basefilename ++ takeExtensions (drop 1 (takeExtensions fname))

-- Given the path to the target and do file relative to the current directory
-- return the absolute path to the do directory and the relative paths from the
-- do directory to the do file, and the do directory to the target file
getTargetRel2Do :: FilePath -> FilePath -> IO (FilePath, FilePath, FilePath)
getTargetRel2Do target doFile = do
  (doDir, doFileName) <- splitFileName <$> makeAbsolute doFile
  targetRel2Do <- makeRelative doDir <$> makeAbsolute target
  return (doDir, doFileName, targetRel2Do)

-- Does the target file or directory exist on the filesystem?
doesTargetExist :: FilePath -> IO Bool
doesTargetExist target = (||) <$> doesFileExist target <*> doesDirectoryExist target

