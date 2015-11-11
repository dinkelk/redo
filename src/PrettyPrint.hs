
module PrettyPrint(putInfoStrLn, putWarningStrLn, putErrorStrLn, putRedoStatus, putUnformattedStrLn) where

import System.Console.ANSI (hSetSGR, SGR(..), ConsoleLayer(..), Color(..), ColorIntensity(..), ConsoleIntensity(..))
import System.IO (hPutStrLn, hPutStr, stderr)

-- Set colors and write some text in those colors.
setConsoleDefault :: IO ()
setConsoleDefault = hSetSGR stderr [Reset]
setConsoleColor :: Color -> IO ()
setConsoleColor color = hSetSGR stderr [SetColor Foreground Vivid color] 
setConsoleBold :: IO ()
setConsoleBold = hSetSGR stderr [SetConsoleIntensity BoldIntensity]
setConsoleFaint :: IO ()
setConsoleFaint = hSetSGR stderr [SetConsoleIntensity FaintIntensity]
setConsoleColorDull :: Color -> IO ()
setConsoleColorDull color = hSetSGR stderr [SetColor Foreground Dull color] 

-- Put string to console in color:
putColorStrLn :: Color -> String -> IO ()
putColorStrLn color string = do setConsoleColor color
                                setConsoleBold 
                                hPutStrLn stderr string
                                setConsoleDefault 

-- Put info, warning, error strings to console:
putUnformattedStrLn :: String -> IO ()
putUnformattedStrLn = hPutStrLn stderr
putInfoStrLn :: String -> IO ()
putInfoStrLn = putColorStrLn Green 
putWarningStrLn :: String -> IO ()
putWarningStrLn = putColorStrLn Yellow 
putErrorStrLn :: String -> IO ()
putErrorStrLn = putColorStrLn Red 

-- Special function to format and print the redo status message of what is being built:
putRedoStatus :: Int -> FilePath -> IO ()
putRedoStatus depth file = do setConsoleColorDull Green 
                              setConsoleFaint
                              hPutStr stderr $ "redo  " ++ concat (replicate depth "  " )
                              setConsoleColor Green
                              setConsoleBold
                              hPutStrLn stderr file
                              setConsoleDefault
