
module PrettyPrint(putRedoUnformatted, putRedoInfo, putRedoWarning, putRedoStatus, putInfoStrLn, putWarningStrLn, putErrorStrLn, putStatusStrLn, putRedoStatus, putUnformattedStrLn) where

import System.IO (hPutStrLn, stderr, hFlush, hSetBuffering, BufferMode(..), stdout)
import Data.Maybe (fromJust, isNothing)
import System.Environment (lookupEnv)

-- ANSI color definitions:
-- import Data.Maybe (isJust)
-- import System.Environment (lookupEnv)
-- import System.IO (hIsTerminalDevice)
-- TODO make these "" if terminal is not smart yes now or on switch
-- isSmartTerminal :: IO Bool
-- isSmartTerminal = do term1 <- lookupEnv "TERM"
--                      term2 <- hIsTerminalDevice stderr
--                      return $ term2 && isJust term1

import FilePathUtil
import Types

red :: String
red = "\x1b[31m"
green :: String
green = "\x1b[32m"
yellow :: String
yellow = "\x1b[33m"
cyan :: String
cyan = "\x1b[36m"
bold :: String
bold = "\x1b[1m"
plain :: String
plain = "\x1b[m"

-- Print function that works well in a threaded environment
putStrBuffered :: String -> IO ()
putStrBuffered string = do hSetBuffering stderr LineBuffering
                           hFlush stdout
                           hPutStrLn stderr $ string
                           hFlush stderr

-- Put string to console in color:
putColorStrLn :: String -> String -> IO ()
putColorStrLn color string = putStrBuffered $ color ++ bold ++ string ++ plain

-- Put info, warning, error strings to console:
putUnformattedStrLn :: String -> IO ()
putUnformattedStrLn = hPutStrLn stderr
putInfoStrLn :: String -> IO ()
putInfoStrLn = putColorStrLn green 
putWarningStrLn :: String -> IO ()
putWarningStrLn = putColorStrLn yellow 
putErrorStrLn :: String -> IO ()
putErrorStrLn = putColorStrLn red 
putStatusStrLn :: String -> IO ()
putStatusStrLn = putColorStrLn cyan

putRedo :: String -> Target -> String -> IO ()
putRedo color target string = do
  depth <- getDepth
  target' <- getRelativeTarget target
  putStrBuffered $ color ++ "redo  " ++ depth ++ bold ++ target' ++ " " ++ string ++ plain

getDepth :: IO String
getDepth = do
  redoDepth' <- lookupEnv "REDO_DEPTH"                -- Depth of recursion for this call to redo
  let redoDepth = if isNothing redoDepth' then 0 else (read (fromJust redoDepth') :: Int) + 1
  return $ concat (replicate redoDepth "  " )

getRelativeTarget :: Target -> IO FilePath
getRelativeTarget target = do
  redoInitPath' <- lookupEnv "REDO_INIT_PATH"         -- Path where redo was initially invoked
  let redoInitPath = fromJust redoInitPath'           -- this should always be set from the first run of redo
  return $ makeRelative' redoInitPath (unTarget target)

putRedoInfo :: Target -> IO ()
putRedoInfo target = putRedo green target ""

putRedoStatus :: Target -> String -> IO ()
putRedoStatus target string = putRedo cyan target ("- " ++ string)

putRedoWarning :: Target -> String -> IO ()
putRedoWarning target string = putRedo yellow target ("- " ++ string)

putRedoError :: Target -> String -> IO ()
putRedoError target string = putRedo red target ("- " ++ string)

putRedoUnformatted :: Target -> String -> IO ()
putRedoUnformatted target string = putRedo "" target ("- " ++ string)




