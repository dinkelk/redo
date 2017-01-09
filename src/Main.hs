{-# LANGUAGE ScopedTypeVariables #-}

-- System imports:
import Control.Monad (unless, when)
import Data.List (intercalate)
import Data.Maybe (isNothing, fromJust, fromMaybe)
import System.Console.GetOpt
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, getProgName, lookupEnv, setEnv)
import System.Exit (exitSuccess, exitFailure, exitWith)
import System.Random (randomRIO)

-- Local imports:
import JobServer
import Database
import PrettyPrint
import Build
import Types

-- Redo options:
data Options = Options {
  help :: Bool,
  dashX :: Bool,
  dashV :: Bool,
  dashD1 :: Bool,
  dashD2 :: Bool,
  keepGoing :: Bool,
  jobs :: Int,
  shuffle :: Bool,
  noColor :: Bool
}

-- Redo default options:
defaultOptions :: Options
defaultOptions = Options {
  help = False,
  dashX = False,
  dashV = False,
  dashD1 = False,
  dashD2 = False,
  keepGoing = False,
  jobs = 1,
  shuffle = False,
  noColor = False 
}

-- Define program options:
-- The arguments to Option are:
-- 1) list of short option characters
-- 2) list of long option strings (without "--")
-- 3) argument descriptor
-- 4) explanation of option for user
options :: [OptDescr (Options -> IO Options)]
options =
  [ Option ['j']      ["jobs"]        (ReqArg setOptJobs "numJobs")  "maximum number of parallel jobs to build at once"
  , Option ['h','H']  ["help"]        (NoArg setHelp)                "show usage details"
  , Option ['x']      ["xtrace"]      (NoArg setDashX)               "print commands as they are executed with variables expanded"
  , Option ['v']      ["verbose"]     (NoArg setDashV)               "print commands as they are read from .do files"
  , Option ['d']      ["debug1"]      (NoArg setDashD1)              "print all .do file calls by redo"
  , Option ['D']      ["debug2"]      (NoArg setDashD2)              "print all calls to redo before they are executed"
  , Option ['V','?']  ["version"]     (NoArg printVersion)           "print the version number"
  , Option ['k']      ["keep-going"]  (NoArg setKeepGoing)           "keep building even if some targets fail"
  , Option ['s']      ["shuffle"]     (NoArg setShuffle)             "randomize the build order to find dependency bugs"
  , Option ['n']      ["no-color"]    (NoArg setNoColor)             "print plain text, disable ANSI terminal color output"
  ]

-- Helper functions for setting the options:
setOptJobs :: String -> Options -> IO Options
setOptJobs cmdLineArg opt = return opt { jobs = read cmdLineArg :: Int }

setHelp :: Options -> IO Options
setHelp opt = return opt { help = True }

setDashX :: Options -> IO Options
setDashX opt = return opt { dashX = True }

setDashV :: Options -> IO Options
setDashV opt = return opt { dashV = True }

setDashD1 :: Options -> IO Options
setDashD1 opt = return opt { dashD1 = True }

setDashD2 :: Options -> IO Options
setDashD2 opt = return opt { dashD2 = True }

setKeepGoing :: Options -> IO Options
setKeepGoing opt = return opt { keepGoing = True }

setShuffle :: Options -> IO Options
setShuffle opt = return opt { shuffle = True }

setNoColor :: Options -> IO Options
setNoColor opt = return opt { noColor = True }

-- Print the program version and license information:
printVersion :: Options -> IO Options
printVersion _ = do putStrLn "Redo 0.1\nThe MIT License (MIT)\nCopyright (c) 2015"
                    exitSuccess

-- Print the program's help details:
printHelp :: String -> [OptDescr a] -> [String] -> IO b
printHelp programName opts errs = if null errs then do putStrLn $ helpStr opts 
                                                       exitSuccess
                                               else ioError (userError (concat errs ++ helpStr opts))
  where helpStr = usageInfo $ "Usage: " ++ programName ++ " [OPTION...] targets..."

-- Helper function to get parse through commandline arguments and return options:
getOptions :: IO (Options, [String])
getOptions = do
  args <- getArgs
  case getOpt Permute options args of
    (o,n,[]  ) -> do o' <- setOptions o
                     return (o',n)
    (_,_,errs) -> do programName <- getProgName
                     printHelp programName options errs 
  where setOptions = foldl (>>=) (return defaultOptions)

-- Main function:
main :: IO ()
main = do 
  
  -- Get program name and arguments:
  progName <- getProgName

  -- Parse options, getting a list of option actions:
  (opts, targets) <- getOptions
  let Options { help = help', 
                dashX = dashX',
                dashV = dashV',
                dashD1 = dashD1',
                dashD2 = dashD2',
                keepGoing = keepGoing',
                jobs = jobs',
                shuffle = shuffle',
                noColor = noColor'} = opts

  -- Show help or version information if asked:
  when help' (printHelp progName options [])
  when keepGoing' (setEnv "REDO_KEEP_GOING" "TRUE")
  when shuffle' (setEnv "REDO_SHUFFLE" "TRUE")
  when noColor' (setEnv "REDO_NO_COLOR" "TRUE")
  when dashD1' (setEnv "REDO_DEBUG_1" "TRUE")
  when dashD2' (setEnv "REDO_DEBUG_2" "TRUE")

  -- If there are shell args, set an environment variable that can be used by all
  -- redo calls after this.
  let shellArgs = intercalate "" [if dashX' then "x" else "",
                                  if dashV' then "v" else ""]
  unless (null shellArgs) (setEnv "REDO_SHELL_ARGS" shellArgs)

  -- Set the redo path variable to the current directory for the first call:
  redoInitPath <- lookupEnv "REDO_INIT_PATH" -- Path where redo was initially invoked
  when (isNothing redoInitPath || null (fromJust redoInitPath)) (setEnv "REDO_INIT_PATH" =<< getCurrentDirectory) 

  -- Get targets to run:
  targetsToRun' <- targetsToRun targets

  -- Print debug2 info if requested:
  debug2Flag <- lookupEnv "REDO_DEBUG_2"
  let debug2 = fromMaybe "" debug2Flag
  when (not (null debug2)) (putUnformattedStrLn $ progName ++ " " ++ unwords (map (unTarget) targetsToRun') )

  -- Run the main:
  mainToRun' <- mainToRun jobs'
  mainToRun' progName targetsToRun'
  where
    -- Shuffle the targets if required:
    targetsToRun targets = do shuffleTargets' <- lookupEnv "REDO_SHUFFLE"
                              let shuffleTargets = fromMaybe "" shuffleTargets'
                              if null shuffleTargets then return targets'
                              else shuffleList targets'
      where targets' = map Target targets
    -- Check if redo is being run from inside of a .do file, or if this is the top level run
    -- Run the correct main accordingly
    mainToRun numJobs = do runFromDoFile <- isRunFromDoFile
                           return $ if runFromDoFile then mainDo else mainTop numJobs

-- The main function for redo run at a top level (outside of a .do file)
mainTop :: Int -> String -> [Target] -> IO()
mainTop numJobs progName targets = do
  -- Setup cache and job server for first run:
  initializeSession
  handle <- initializeJobServer numJobs

  -- Perform the proper action based on the program name:
  case progName of 
    "redo" -> exitWith' handle =<< redo targets'
    "redo-ifchange" -> exitWith' handle =<< redoIfChange targets
    -- redo-ifcreate and redo-always should only be run inside of a .do file
    "redo-ifcreate" -> runOutsideDoError progName 
    "redo-always" -> runOutsideDoError progName 
    _ -> return ()
  where
    -- If just 'redo' is run, then assume the default target as 'all'
    targets' = if null targets then [Target "all"] else targets
    -- Print warning message if redo-always or redo-ifcreate are run outside of a .do file
    runOutsideDoError :: String -> IO ()
    runOutsideDoError program = do putWarningStrLn $ "Warning: '" ++ program ++ "' can only be invoked inside of a .do file."
                                   exitFailure
    -- Clear out any temp files from this session
    exitWith' handle code = clearJobServer handle >> clearRedoTempDirectory >> exitWith code

-- The main function for redo run within a .do file
mainDo :: String -> [Target] -> IO ()
mainDo progName targets =
  -- Perform the proper action based on the program name:
  case progName of 
    "redo" -> exitWith =<< redo targets
    "redo-ifchange" -> do exitCode <- redoIfChange targets
                          storeIfChangeDependencies targets
                          exitWith exitCode
    "redo-ifcreate" -> storeIfCreateDependencies targets
    "redo-always" -> storeAlwaysDependency
    _ -> return ()

-- Randomly shuffle the order of a list:
-- http://en.literateprograms.org/Fisher-Yates_shuffle_(Haskell)
shuffleList :: [a] -> IO [a]
shuffleList lst = shuffle' lst []
  where
    shuffle' [] acc = return acc
    shuffle' l acc =
      do k <- randomRIO (0, length l - 1)
         let (lead, x:xs) = splitAt k l
         shuffle' (lead ++ xs) (x:acc)
