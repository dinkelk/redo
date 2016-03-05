{-# LANGUAGE ScopedTypeVariables #-}

module JobServer (initializeJobServer, getJobServer, clearJobServer, runJobs, runJob, waitOnJobs,
                  printJobServerHandle) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, MVar, threadDelay, forkOS)
import Control.Exception.Base (assert)
import Control.Exception (catch, SomeException(..))
import Foreign.C.Types (CInt)
import System.Environment (getEnv, setEnv)
import System.Posix.IO (createPipe, fdWrite, fdRead, FdOption(..), setFdOption, closeFd)
import System.Posix.Types (Fd(..), ByteCount)
import System.IO (hPutStrLn, stderr)

newtype JobServerHandle = JobServerHandle { unJobServerHandle :: (Fd, Fd, [MVar Token]) }
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
  setEnv "MAKEFLAGS" $ show readEnd ++ ", " ++ show writeEnd
  
  -- Return the read and write ends of the pipe:
  return $ JobServerHandle (readEnd, writeEnd, [])
  where tokens = concat $ map show $ take tokensToWrite [(1::Integer)..]
        tokensToWrite = n-1

getJobServer :: IO JobServerHandle
getJobServer = do flags <- getEnv "MAKEFLAGS"
                  let handle = handle' flags
                  return $ JobServerHandle $ (Fd $ handle !! 0, Fd $ handle !! 1, [])
  where handle' flags = map convert (splitBy ',' flags)
        convert a = read a :: CInt

clearJobServer :: JobServerHandle -> IO ()
clearJobServer handle = safeCloseFd w >> safeCloseFd r
  where safeCloseFd fd = catch (closeFd fd) (\(_ :: SomeException) -> return ()) 
        (r, w, _) = unJobServerHandle handle

-- Given a list of IO () jobs, run them when a space on the job server is
-- available.
runJobs :: JobServerHandle -> [IO ()] -> IO ()
runJobs _ [] = return ()
runJobs _ [j] = j
runJobs handle (j:jobs) = maybe (j >> runJobs handle jobs) forkJob =<< getToken r
  where 
    (r, w, _) = unJobServerHandle handle
    forkJob token = do
      hPutStrLn stderr $ "read " ++ unToken token ++ " from pipe. "

      -- Fork new thread to run job:
      m <- newEmptyMVar
      --hPutStrLn stderr $ "fork process " ++ unToken token
      -- consider using fork finally
      -- consider putting thread id in handle so that it can be killed on error
      _ <- forkOS $ runForkedJob m w j
      putMVar m token

      -- Run the rest of the jobs:
      runJobs handle jobs

      -- Wait on my forked job:
      --hPutStrLn stderr $ "waiting on " ++ unToken token
      _ <- takeMVar m
      return ()
      --hPutStrLn stderr $ "reaped " ++ unToken returnedToken

runJob :: JobServerHandle -> IO () -> IO JobServerHandle
runJob handle j = maybe (j >> return handle) forkJob =<< getToken r
  where 
    (r, w, mvars) = unJobServerHandle handle
    forkJob token = do
      hPutStrLn stderr $ "read " ++ unToken token ++ " from pipe. "

      -- Fork new thread to run job:
      m <- newEmptyMVar
      --hPutStrLn stderr $ "fork process " ++ unToken token
      -- consider using fork finally
      _ <- forkOS $ runForkedJob m w j
      putMVar m token
      return $ JobServerHandle (r, w, mvars++[m])

printJobServerHandle :: JobServerHandle -> IO ()
printJobServerHandle handle = hPutStrLn stderr $ "handle: (" ++ show r ++ ", " ++ show w ++ ", len " ++ show (length mvars) ++ ")"
  where (r, w, mvars) = unJobServerHandle handle

runForkedJob :: MVar (Token) -> Fd -> IO () -> IO ()
runForkedJob m w job = do 
  token <- takeMVar m
  --hPutStrLn stderr $ "-- starting job with token: " ++ unToken token
  job
  --hPutStrLn stderr $ "-- finished job with token: " ++ unToken token

  -- Return the token:
  returnToken w token
   
  -- Signal that I have finished:
  putMVar m token
  return ()

-- todo return status here
waitOnJobs :: JobServerHandle -> IO ()
waitOnJobs handle = mapM_ takeMVar mvars
  where
    (_, _, mvars) = unJobServerHandle handle

-- Get a token if one is available, otherwise return Nothing:
getToken :: Fd -> IO (Maybe Token)
getToken fd = catch (readPipe) (\(_ :: SomeException) -> return Nothing) 
  where readPipe = do (token, byteCount) <- fdRead fd 1
                      assert_ $ countToInt byteCount == 1
                      return $ Just $ Token $ token

-- Return a token to the pipe:
returnToken :: Fd -> Token -> IO ()
returnToken fd token = do byteCount <- fdWrite fd (unToken token)
                          assert_ $ countToInt byteCount == 1

-- Convenient assert function:
assert_ :: Monad m => Bool -> m ()
assert_ c = assert c (return ())

-- Conversion helper for ByteCount type:
countToInt :: ByteCount -> Int
countToInt a = fromIntegral a

splitBy :: Char -> String -> [String]
splitBy delimiter = foldr f [[]] 
  where f c l@(x:xs) | c == delimiter = []:l
                     | otherwise = (c:x):xs
        f _ [] = []

-- Main function:
main :: IO ()
main = do handle <- initializeJobServer 4
          printJobServerHandle handle
          runJobs handle jobs
          hPutStrLn stderr "--------------------------------------------------------------------"
          handle2 <- getJobServer
          printJobServerHandle handle2
          handles <- mapM (runJob handle2) jobs
          mapM_ printJobServerHandle handles
          mapM_ waitOnJobs handles
          clearJobServer handle
  where jobs = [exampleLongJob "A", exampleJob "B", exampleLongJob "C", 
                exampleJob "D", exampleJob "E", exampleJob "F",
                exampleJob "G", exampleJob "H", exampleJob "I",
                exampleJob "J", exampleJob "K", exampleJob "L"]

exampleJob :: String -> IO ()
exampleJob n = do hPutStrLn stderr $ ".... Running job: " ++ n
                  threadDelay 1000000
                  hPutStrLn stderr $ ".... Finishing job: " ++ n

exampleLongJob :: String -> IO ()
exampleLongJob n = do hPutStrLn stderr $ ".... Running job: " ++ n
                      threadDelay 10000000
                      hPutStrLn stderr $ ".... Finishing job: " ++ n


