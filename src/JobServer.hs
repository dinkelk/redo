{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

module JobServer (initializeJobServer, getJobServer, clearJobServer, runJobs, JobServerHandle,
                  waitOnJob, runJob, tryWaitOnJob, returnToken, getToken, Token(..)) where

import Control.Applicative ((<$>))
import Control.Exception.Base (assert)
import Control.Exception (catch, SomeException(..))
import Foreign.C.Types (CInt)
import System.Environment (getEnv, setEnv)
import System.Exit (ExitCode(..))
import System.Posix.IO (fdWrite, fdRead, closeFd)
import System.Posix.Types (Fd(..), ByteCount, ProcessID)
import System.Posix.Process (forkProcess, getProcessStatus, ProcessStatus(..))
import System.Posix.Files (createNamedPipe, ownerReadMode, ownerWriteMode, namedPipeMode, unionFileModes)
import System.Posix.IO (openFd, OpenMode(..), defaultFileFlags, OpenFileFlags(..))

import Database

newtype JobServerHandle = JobServerHandle { unJobServerHandle :: (Fd, Fd, Fd) }
newtype Token = Token { unToken :: String } deriving (Eq, Show)

initializeJobServer :: Int -> IO JobServerHandle
initializeJobServer n = do 
  -- Create a named pipe:
  pipeName <- getJobServerPipe
  createNamedPipe pipeName (unionFileModes (unionFileModes ownerReadMode ownerWriteMode) namedPipeMode)

  -- Open read and write ends of named pipe:
  -- Note: the order in which this is done is important, otherwise this function will
  -- block forever. 
  -- See: http://stackoverflow.com/questions/5782279/why-does-a-read-only-open-of-a-named-pipe-block
  readNonBlocking <- openFd pipeName ReadOnly Nothing readNonBlockFlags
  write <- openFd pipeName WriteOnly Nothing defaultFileFlags
  readBlocking <- openFd pipeName ReadOnly Nothing defaultFileFlags

  assert_ $ readBlocking >= 0
  assert_ $ readNonBlocking >= 0
  assert_ $ write >= 0
  assert_ $ readBlocking /= readNonBlocking
  assert_ $ readNonBlocking /= write
  assert_ $ write /= readBlocking
  

  -- Write the tokens to the pipe:
  byteCount <- fdWrite write tokens
  assert_ $ countToInt byteCount == tokensToWrite

  -- Set an environment variable to store the handle for 
  -- other programs that might use this server:
  setEnv "REDO_JOB_SERVER_PIPE" $ show readBlocking ++ ", " ++
                                  show readNonBlocking ++ ", " ++
                                  show write

  -- Return the read and write ends of the pipe:
  return $ JobServerHandle (readBlocking, readNonBlocking, write)
  where tokens = concatMap show $ take tokensToWrite [(1::Integer)..]
        tokensToWrite = n-1
        readNonBlockFlags = 
          OpenFileFlags { nonBlock = True, append = False, 
                          exclusive = False, noctty = False, 
                          trunc = False }

-- Get a job server that has already been created:
getJobServer :: IO JobServerHandle
getJobServer = do flags <- getEnv "REDO_JOB_SERVER_PIPE"
                  let handle = handle' flags
                  return $ JobServerHandle (Fd $ head handle, Fd $ handle !! 1, Fd $ handle !! 2)
  where handle' flags = map convert (splitBy ',' flags)
        convert a = read a :: CInt

-- Clear the job server by closing all the open file descriptors associated 
-- with it.
clearJobServer :: JobServerHandle -> IO ()
clearJobServer handle = safeCloseFd w >> safeCloseFd r >> safeCloseFd r'
  where safeCloseFd fd = catch (closeFd fd) (\(_ :: SomeException) -> return ()) 
        (r', r, w) = unJobServerHandle handle

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

-- Run a single job. Fork it if a token is avalable, otherwise run it on 
-- the current thread.
runJob :: JobServerHandle -> IO ExitCode -> IO (Either ProcessID ExitCode)
runJob handle j = maybe runJob' forkJob =<< tryGetToken handle
  where 
    forkJob token = do
      processStatus <- forkProcess $ runForkedJob handle token j
      return $ Left processStatus
    runJob' = do ret <- j
                 return $ Right ret

-- Run a job and then return the token associated with it.
runForkedJob :: JobServerHandle -> Token -> IO ExitCode -> IO ()
runForkedJob handle token job = do 
  _ <- job
  returnToken handle token
  return ()

-- Wait on job to finish, and return the exit code when it does:
waitOnJob :: ProcessID -> IO ExitCode
waitOnJob pid = (maybe (ExitFailure 1) getExitCode) <$> (getProcessStatus True False pid)

-- Return a job's exit code if it's finished, otherwise return
-- nothing.
tryWaitOnJob :: ProcessID -> IO (Maybe ExitCode)
tryWaitOnJob pid = (getExitCode <$>) <$> (getProcessStatus False False pid)

-- Get the exit code from a process status:
getExitCode :: ProcessStatus -> ExitCode
getExitCode (Exited code) = code
getExitCode (Terminated _ _) = ExitFailure 1
getExitCode (Stopped _) = ExitFailure 1

-- Get a token if one is available, otherwise return Nothing:
tryGetToken :: JobServerHandle -> IO (Maybe Token)
tryGetToken handle = catch (Just <$> (readToken r)) (\(_ :: SomeException) -> return Nothing) 
  where (_, r, _) = unJobServerHandle handle

--tryGetToken :: JobServerHandle -> IO (Maybe Token)
--tryGetToken handle = do fdReady <- hSelect [r] [] []
--                        assert_ $ fdRead >= 0
--                        if rdReady < 1 then return Nothing
--                        else do token <- readToken r
--                                return $ Just $ token
--  where (r, _) = unJobServerHandle handle


-- Wait for a token to become available and then return it:
getToken :: JobServerHandle -> IO Token
getToken handle = readToken r
  where (r, _, _) = unJobServerHandle handle

-- Blocking read the next token from the pipe:
readToken :: Fd -> IO Token
readToken fd = do (token, byteCount) <- fdRead fd 1
                  assert_ $ countToInt byteCount == 1
                  return $ Token token

-- Return a token to the pipe:
returnToken :: JobServerHandle -> Token -> IO ()
returnToken handle token = do byteCount <- fdWrite w (unToken token)
                              assert_ $ countToInt byteCount == 1
  where (_, _, w) = unJobServerHandle handle

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
