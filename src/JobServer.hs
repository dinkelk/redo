{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

module JobServer (initializeJobServer, getJobServer, clearJobServer, runJobs, JobServerHandle,
                  waitOnJob, runJob, tryWaitOnJob) where

import Control.Exception.Base (assert)
import Control.Exception (catch, SomeException(..))
import Foreign.C.Types (CInt)
import System.Environment (getEnv, setEnv)
import System.Exit (ExitCode(..))
import System.Posix.IO (createPipe, fdWrite, fdRead, FdOption(..), setFdOption, closeFd)
import System.Posix.Types (Fd(..), ByteCount, ProcessID)
import System.Posix.Process (forkProcess, getProcessStatus, ProcessStatus(..))

newtype JobServerHandle = JobServerHandle { unJobServerHandle :: (Fd, Fd) }
newtype Token = Token { unToken :: String } deriving (Eq, Show)

initializeJobServer :: Int -> IO JobServerHandle
initializeJobServer n = do 
  -- Create the pipe: 
  (readEnd, writeEnd) <- createPipe
  assert_ $ readEnd >= 0
  assert_ $ writeEnd >= 0
  assert_ $ readEnd /= writeEnd

  -- Make the read end of the pipe non-blocking:
  setFdOption readEnd NonBlockingRead True

  -- Write the tokens to the pipe:
  byteCount <- fdWrite writeEnd tokens
  assert_ $ countToInt byteCount == tokensToWrite

  -- Set an environment variable to store the handle for 
  -- other programs that might use this server:
  -- TODO: make compatible with make? Or give up on that?
  setEnv "MAKEFLAGS" $ show readEnd ++ ", " ++ show writeEnd
  
  -- Return the read and write ends of the pipe:
  return $ JobServerHandle (readEnd, writeEnd)
  where tokens = concatMap show $ take tokensToWrite [(1::Integer)..]
        tokensToWrite = n-1

getJobServer :: IO JobServerHandle
getJobServer = do flags <- getEnv "MAKEFLAGS"
                  let handle = handle' flags
                  return $ JobServerHandle (Fd $ head handle, Fd $ handle !! 1)
  where handle' flags = map convert (splitBy ',' flags)
        convert a = read a :: CInt

clearJobServer :: JobServerHandle -> IO ()
clearJobServer handle = safeCloseFd w >> safeCloseFd r
  where safeCloseFd fd = catch (closeFd fd) (\(_ :: SomeException) -> return ()) 
        (r, w) = unJobServerHandle handle

-- Given a list of IO () jobs, run them when a space on the job server is
-- available.
runJobs :: JobServerHandle -> [IO ExitCode] -> IO [ExitCode]
runJobs _ [] = return []
runJobs _ [j] = do ret <- j 
                   return [ret]
runJobs handle (j:jobs) = maybe runJob' forkJob =<< tryGetToken handle
  where 
    forkJob token = do
      -- Fork new thread to run job:
      processId <- forkProcess $ runForkedJob handle token j
      -- Run the rest of the jobs:
      rets <- runJobs handle jobs
      maybe (return $ ExitFailure 1 : rets) (returnExitCode rets) 
        =<< getProcessStatus True False processId
    runJob' = do ret1 <- j
                 rets <- runJobs handle jobs
                 return $ ret1:rets 
    returnExitCode rets processStatus = return $ code:rets
      where code = getExitCode processStatus

runJob :: JobServerHandle -> IO ExitCode -> IO (Either ProcessID ExitCode)
runJob handle j = maybe runJob' forkJob =<< tryGetToken handle
  where 
    forkJob token = do
      processStatus <- forkProcess $ runForkedJob handle token j
      return $ Left processStatus
    runJob' = do ret <- j
                 return $ Right ret

runForkedJob :: JobServerHandle -> Token -> IO ExitCode -> IO ()
runForkedJob handle token job = do 
  _ <- job
  returnToken handle token
  return ()

-- Wait on job to finish, and return the exit code when it does:
waitOnJob :: ProcessID -> IO ExitCode
waitOnJob pid = fmap (maybe (ExitFailure 1) getExitCode) (getProcessStatus True False pid)

-- Return a job's exit code if it's finished, otherwise return
-- nothing.
tryWaitOnJob :: ProcessID -> IO (Maybe ExitCode)
tryWaitOnJob pid = fmap (fmap getExitCode) (getProcessStatus False False pid)

-- Get the exit code from a process status:
getExitCode :: ProcessStatus -> ExitCode
getExitCode (Exited code) = code
getExitCode (Terminated _ _) = ExitFailure 1
getExitCode (Stopped _) = ExitFailure 1

-- Get a token if one is available, otherwise return Nothing:
tryGetToken :: JobServerHandle -> IO (Maybe Token)
tryGetToken handle = catch readPipe (\(_ :: SomeException) -> return Nothing) 
  where readPipe = do (token, byteCount) <- fdRead r 1
                      assert_ $ countToInt byteCount == 1
                      return $ Just $ Token token
        (r, _) = unJobServerHandle handle

-- Wait for a token to become available and then return it:
--getToken :: JobServerHandle -> IO ()

-- Return a token to the pipe:
returnToken :: JobServerHandle -> Token -> IO ()
returnToken handle token = do byteCount <- fdWrite w (unToken token)
                              assert_ $ countToInt byteCount == 1
  where (_, w) = unJobServerHandle handle

-- Convenient assert function:
assert_ :: Monad m => Bool -> m ()
assert_ c = assert c (return ())

-- Conversion helper for ByteCount type:
countToInt :: ByteCount -> Int
countToInt = fromIntegral

splitBy :: Char -> String -> [String]
splitBy delimiter = foldr f [[]] 
  where f c l@(x:xs) | c == delimiter = []:l
                     | otherwise = (c:x):xs
        f _ [] = []
