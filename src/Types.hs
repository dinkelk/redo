{-# LANGUAGE ScopedTypeVariables #-}

module Types(stampTarget, safeStampTarget, doesTargetExist, doesDoFileExist, findDoFile, Stamp(..), DoFile(..), Target(..)) where

import Control.Exception (catch, SomeException(..))
import Control.Monad (liftM, filterM)
import Data.Bool (bool)
import Data.Maybe (isNothing, listToMaybe)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath (takeExtensions, dropExtension, dropExtensions, isDrive, splitFileName, (</>), takeDirectory, pathSeparator)
import System.Posix.Files (getFileStatus, modificationTimeHiRes)
import Data.Time.Clock.POSIX (POSIXTime)

import FilePathUtil

-- This module provides basic types for redo
---------------------------------------------------------------------
-- Basic Redo Type Definitions:
---------------------------------------------------------------------
newtype Stamp = Stamp { unStamp :: POSIXTime } deriving (Show, Eq, Ord) -- Timestamp or hash stamp of a file
newtype DoFile = DoFile { unDoFile :: FilePath } deriving (Show, Eq) -- The absolute path to a do file
newtype Target = Target { unTarget :: FilePath } deriving (Show, Eq) -- The absolute path to a target file

-- Common functions that use these types:
---------------------------------------------------------------------
-- Generate stamps from targets
---------------------------------------------------------------------
-- Get the stamp of the target which marks it's current status
stampTarget :: Target -> IO Stamp 
stampTarget = getTimeStamp

-- Get the stamp of the target if it exists, otherwise return Nothing
safeStampTarget :: Target -> IO (Maybe Stamp)
safeStampTarget target = catch (Just <$> stampTarget target) (\(_ :: SomeException) -> return Nothing)

-- Get the target timestamp (old version, now using real POSIXTime instead of string, below)
-- newtype Stamp = Stamp { unStamp :: String } deriving (Show, Eq) -- Timestamp or hash stamp of a file
-- getTimeStamp :: Target -> IO Stamp
-- getTimeStamp target = do
--   st <- getFileStatus $ unTarget target
--   return $ Stamp $ show (modificationTimeHiRes st) ++ show (fileID st) ++ show (fileSize st)

-- Get the target timestamp
getTimeStamp :: Target -> IO Stamp
getTimeStamp target = do
  st <- getFileStatus $ unTarget target
  return $ Stamp $ modificationTimeHiRes st

-- Hash the file (no longer supported, using timestamps for speed)
-- getTargetHashStamp :: Target -> IO Stamp
-- getTargetHashStamp file = Stamp <$> hash `liftM` unStamp <$> readMetaFile (unTarget file)

---------------------------------------------------------------------
-- Existance functions:
---------------------------------------------------------------------
-- Does a do file exist on the file system?
doesDoFileExist :: DoFile -> IO Bool
doesDoFileExist doFile = doesFileExist $ unDoFile doFile 

-- Does a target exist on the file system?
doesTargetExist :: Target -> IO Bool
doesTargetExist target = (||) <$> doesFileExist filePath <*> doesDirectoryExist filePath
  where filePath = unTarget target

---------------------------------------------------------------------
-- Find do files.
---------------------------------------------------------------------
-- Returns the absolute path to the do file given the absolute path to the target:
findDoFile :: Target -> IO (Maybe DoFile)
findDoFile absTarget = do 
  let (targetDir, targetName) = splitFileName $ unTarget absTarget
  let targetDo = DoFile $ removeDotDirs $ unTarget absTarget ++ ".do" 
  bool (defaultDoPath targetDir targetName) (return $ Just targetDo) =<< doesDoFileExist targetDo
  where
    -- Try to find matching .do file by checking directories upwards of "." until a suitable match is 
    -- found or "/" is reached.
    defaultDoPath :: FilePath -> FilePath -> IO (Maybe DoFile)
    defaultDoPath absPath' name = do
      let absPath = if last absPath' == pathSeparator then takeDirectory absPath' else absPath'
      doFile <- listToMaybe `liftM` filterM doesDoFileExist (candidates absPath name)
      if isNothing doFile && not (isDrive absPath) then defaultDoPath (takeDirectory absPath) name
      else return doFile
    -- List the possible default.do file candidates relative to the given path:
    candidates path name = map (DoFile . (path </>)) (defaults name) 
    defaults name = map (++ ".do") (getDefaultDo $ "default" ++ takeExtensions name)
    -- Form all possible matching default.do files in order of preference:
    getDefaultDo :: FilePath -> [FilePath]
    getDefaultDo filename = filename : if smallfilename == filename then [] else getDefaultDo $ dropFirstExtension filename
      where smallfilename = dropExtension filename
            basefilename = dropExtensions filename
            dropFirstExtension fname = basefilename ++ takeExtensions (drop 1 (takeExtensions fname))

