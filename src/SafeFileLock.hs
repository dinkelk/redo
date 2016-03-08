{-# LANGUAGE ScopedTypeVariables #-}

module SafeFileLock (FileLockHandle, createFileLock, tryFileLock, waitFileLock, releaseFileLock) where

import Control.Exception.Base (assert)
import System.FilePath ((</>))
import System.FileLock (SharedExclusive(..), FileLock, lockFile, tryLockFile, unlockFile)
import Control.Concurrent (myThreadId, newMVar, putMVar, takeMVar, tryTakeMVar, MVar)

import Database
import FilePathUtil
import Types
import PrettyPrint

---------------------------------------------------------------------
-- Type Definitions:
---------------------------------------------------------------------
newtype FileLockHandle = FileLockHandle { unFileLockHandle :: MVar FilePath }

---------------------------------------------------------------------
-- Lock File Functions:
---------------------------------------------------------------------
-- The haskell System.FileLock package
--
--   https://hackage.haskell.org/package/filelock-0.1.0.1/docs/System-FileLock.html
--
-- uses flock(2)
--
--   http://linux.die.net/man/2/flock

-- which does not behave correctly for locking between threads. It is
-- designed for inter-process communication. Since redo uses threads
-- heavily, as well as processes, we need some extra protection. This
-- file lock interface uses an MVar as a mutex to protect file access
-- among threads. The mutex protects the FileLock interface which will
-- protect file access among processes. Pretty heavy weight, but can
-- deadlock without it.
--
-- Return the lock file handle for a target:
createFileLock :: Target -> IO FileLockHandle
createFileLock target = do
  path <- getFileLockPath
  mutex <- newMVar path
  return $ FileLockHandle mutex
  where getFileLockPath :: IO FilePath
        getFileLockPath = do 
          key <- getKey target
          lockFileDir <- getLockFileDatabase key
          safeCreateDirectoryRecursive lockFileDir 
          return $ lockFileDir </> "l"

waitFileLock :: FileLockHandle -> IO FileLock
waitFileLock handle = do
  id_ <- myThreadId
  putStatusStrLn $ "waiting on mvar " ++ show id_
  path <- takeMVar mutex
  putStatusStrLn $ "waiting on: " ++ path
  assert_ $ not $ null path
  lockFile path Exclusive 
  where mutex = unFileLockHandle handle

tryFileLock :: FileLockHandle -> IO (Maybe FileLock)
tryFileLock handle = do
  maybe (return Nothing) (tryLockFile') =<< tryTakeMVar mutex
  where mutex = unFileLockHandle handle
        tryLockFile' path = do
          assert_ $ not $ null path
          maybe (putMVar mutex path >> return Nothing)
                (returnFileLock) =<< tryLockFile path Exclusive 
        returnFileLock lock = return $ Just lock

releaseFileLock :: FileLockHandle -> FileLock -> IO ()
releaseFileLock handle lock = do
  id_ <- myThreadId
  unlockFile lock
  putStatusStrLn $ "releaseing " ++ show id_
  putMVar mutex ""
  where mutex = unFileLockHandle handle

-- Convenient assert function:
assert_ :: Monad m => Bool -> m ()
assert_ c = assert c (return ())

