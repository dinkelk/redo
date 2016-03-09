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
runJobs handle (j:jobs) = maybe runJob' forkJob =<< getToken r
  where 
    (r, w) = unJobServerHandle handle
    forkJob token = do
      -- Fork new thread to run job:
      processId <- forkProcess $ runForkedJob token w j
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
runJob handle j = maybe runJob' forkJob =<< getToken r
  where 
    (r, w) = unJobServerHandle handle
    forkJob token = do
      processStatus <- forkProcess $ runForkedJob token w j
      return $ Left processStatus
    runJob' = do ret <- j
                 return $ Right ret

runForkedJob :: Token -> Fd -> IO ExitCode -> IO ()
runForkedJob token w job = do 
  _ <- job
  returnToken w token
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
getToken :: Fd -> IO (Maybe Token)
getToken fd = catch readPipe (\(_ :: SomeException) -> return Nothing) 
  where readPipe = do (token, byteCount) <- fdRead fd 1
                      assert_ $ countToInt byteCount == 1
                      return $ Just $ Token token

-- Return a token to the pipe:
returnToken :: Fd -> Token -> IO ()
returnToken fd token = do byteCount <- fdWrite fd (unToken token)
                          assert_ $ countToInt byteCount == 1

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
