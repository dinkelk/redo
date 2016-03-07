
module PrettyPrint(putInfoStrLn, putWarningStrLn, putErrorStrLn, putStatusStrLn, putRedoStatus, putUnformattedStrLn) where

import System.IO (hPutStrLn, stderr, hFlush, hSetBuffering, BufferMode(..), stdout)

-- ANSI color definitions:
-- TODO make these "" if terminal is not smart
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

-- Special function to format and print the redo status message of what is being built:
putRedoStatus :: Int -> FilePath -> IO ()
putRedoStatus depth file = putStrBuffered $ green ++ "redo  " ++ concat (replicate depth "  " ) ++ bold ++ file ++ plain
