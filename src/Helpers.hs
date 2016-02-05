{-# LANGUAGE ScopedTypeVariables #-}

module Helpers(performActionInDir) where

import System.Directory (setCurrentDirectory, getCurrentDirectory)
import Control.Exception (catch, SomeException(..))
import System.FilePath ((</>))
import System.Exit (exitFailure)
import PrettyPrint

-- This applies a function to a target in the directory provided and then
-- returns the current directory to the starting directory:
performActionInDir :: FilePath -> (FilePath -> IO ()) -> FilePath -> IO ()
performActionInDir dir action target = do
  topDir <- getCurrentDirectory
  --redoTarget' <- lookupEnv "REDO_TARGET"
  --case (redoTarget') of 
  --  (Just redoTarget) -> hPutStrLn stderr $ "... redoing " ++ redoTarget ++ "* -> " ++ (target)
  --  (Nothing) -> hPutStrLn stderr $ "... redoing " ++ target ++ "  -> " ++ (target)
  catch (setCurrentDirectory dir) (\(_ :: SomeException) -> do 
    putErrorStrLn $ "Error: No such directory " ++ topDir </> dir
    exitFailure)
  action target
  setCurrentDirectory topDir
