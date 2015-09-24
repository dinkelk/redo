import Control.Monad (filterM, liftM)
import Control.Exception (catch, SomeException)
import System.Directory (renameFile, removeFile, doesFileExist)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.FilePath (replaceBaseName, hasExtension, takeBaseName)
import System.IO (hPutStrLn, stderr)
import System.Process (createProcess, waitForProcess, shell)
import Data.Bool (bool)

main :: IO ()
--main = do
--  args <- getArgs
--  mapM_ redo args
main = mapM_ redo =<< getArgs

redo :: String -> IO ()
redo target = do 
  maybe printMissingPath redo' =<< redoPath target
  where
    printMissingPath = error $ "No .do file found for target '" ++ target ++ "'"
    redo' :: FilePath -> IO ()
    redo' path = do
      (_, _, _, processHandle) <- createProcess $ shell $ command
      exit <- waitForProcess processHandle
      case exit of  
        ExitSuccess -> do catch (renameFile tmp3 target) handler1
        ExitFailure code -> do hPutStrLn stderr $ "\n" ++ "Redo script exited with non-zero exit code: " ++ show code
      -- Remove the temporary files:
      removeTempFile tmp3
      removeTempFile tmpStdout
      where 
        -- Temporary file names:
        tmp3 = target ++ ".redo-tmp" -- this temp file gets passed as $3 and is written to by programs that do not print to stdout
        tmpStdout = target ++ ".redo-tmp2" -- this temp file captures what gets written to stdout
        -- Pass redo script 3 arguments:
        -- $1 - the target name
        -- $2 - the target basename
        -- $3 - the temporary target name
        command = unwords ["sh ", path, target, takeBaseName target, tmp3, ">", tmpStdout]
        -- If renaming the tmp3 fails, let's try renaming tmpStdout:
        handler1 :: SomeException -> IO ()
        handler1 ex = do catch (renameFile tmpStdout target) handler2
        -- Renaming totally failed, lets alert the user:
        handler2 :: SomeException -> IO ()
        handler2 ex = do hPutStrLn stderr $ "Redo could not copy results from temporary file"
        -- Function to check if the temp file exists, and if it does, remove it:
        removeTempFile :: FilePath -> IO ()
        removeTempFile file = bool (return ()) (removeFile file) =<< doesFileExist file
      

-- Take file path of target and return file path of redo script:
redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = safeHead `liftM` filterM doesFileExist candidates
 -- existingCandidates <- filterM doesFileExist candidates
  --return $ safeHead existingCandidates
  where candidates =  [target ++ ".do"] ++ if hasExtension target then [replaceBaseName target "default" ++ ".do"] else []
        safeHead [] = Nothing
        safeHead (x:_) = Just x
