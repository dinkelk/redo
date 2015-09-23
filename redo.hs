import Control.Monad (filterM)
import System.Directory (renameFile, removeFile, doesFileExist)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.FilePath (replaceBaseName, hasExtension)
import System.IO (hPutStrLn, stderr)
import System.Process (createProcess, waitForProcess, shell)

main :: IO ()
main = do
  args <- getArgs
  mapM_ redo args

redo :: String -> IO ()
redo target = do 
  let tmp = target ++ ".redo-tmp"
  path <- redoPath target
  case path of
    Nothing -> error $ "No .do file found for target '" ++ target ++ "'"
    Just path -> do
      let command = "sh " ++ path ++ " - - " ++ tmp
      (_, _, _, processHandle) <- createProcess $ shell $ command
      exit <- waitForProcess processHandle
      case exit of 
        ExitSuccess -> renameFile tmp target 
        ExitFailure code -> do hPutStrLn stderr $ "\n" ++ "Redo script exited with non-zero exit code: " ++ show code
                               removeFile tmp

-- Take file path of target and return file path of redo script:
redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = do 
  existingCandidates <- filterM doesFileExist candidates
  return $ safeHead existingCandidates
  where candidates =  [target ++ ".do"] ++ if hasExtension target then [replaceBaseName target "default.do"] else []
        safeHead [] = Nothing
        safeHead (x:_) = Just x
