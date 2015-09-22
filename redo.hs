import System.Environment (getArgs)
import System.Process (createProcess, waitForProcess, shell)

main :: IO ()
main = do
  args <- getArgs
  mapM_ redo args

redo :: String -> IO ()
redo target = do 
  (_, _, _, processHandle) <- createProcess $ shell $ "sh " ++ target ++ ".do"
  _ <- waitForProcess processHandle
  return ()
