import Control.Monad (filterM, liftM)
import Control.Exception (catch, SomeException)
import System.Directory (renameFile, removeFile, doesFileExist)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.FilePath (replaceBaseName, hasExtension, takeBaseName)
import System.IO (hPutStrLn, stderr)
import System.Process (createProcess, waitForProcess, shell)

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
    tmp3 = target ++ ".redo-tmp" -- this temp file gets passed as $3 and is written to by programs that do not print to stdout
    tmpStdout = target ++ ".redo-tmp2" -- this temp file captures what gets written to stdout
    redo' :: FilePath -> IO ()
    redo' path = do
      (_, _, _, processHandle) <- createProcess $ shell $ command
      exit <- waitForProcess processHandle
      case exit of  
        -- try to copy temp3 file
        -- if that fails, then copy the tempStdout file
        ExitSuccess -> do catch (renameFile tmp3 target) handler1
                          where
                            handler1 :: SomeException -> IO ()
                            handler1 ex = do catch (renameFile tmpStdout target) handler2
                            handler2 :: SomeException -> IO ()
                            handler2 ex = do hPutStrLn stderr $ "Redo could not copy results from temporary file"
        ExitFailure code -> do hPutStrLn stderr $ "\n" ++ "Redo script exited with non-zero exit code: " ++ show code
      -- Remove the temporary files:
      tmp3Exists <- doesFileExist tmp3 
      if tmp3Exists then removeFile tmp3 else return ()
      tmpStdoutExists <- doesFileExist tmpStdout 
      if tmpStdoutExists then removeFile tmpStdout else return ()
      where 
        -- Pass redo script 3 arguments:
        -- $1 - the target name
        -- $2 - the target basename
        -- $3 - the temporary target name
        command = "sh " ++ path ++ " " ++ target ++ " " ++ takeBaseName target ++ " " ++ tmp3 ++ " > " ++ tmpStdout
       

-- Take file path of target and return file path of redo script:
redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = safeHead `liftM` filterM doesFileExist candidates
 -- existingCandidates <- filterM doesFileExist candidates
  --return $ safeHead existingCandidates
  where candidates =  [target ++ ".do"] ++ if hasExtension target then [replaceBaseName target "default" ++ ".do"] else []
        safeHead [] = Nothing
        safeHead (x:_) = Just x
