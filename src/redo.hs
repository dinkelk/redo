-- adding StandAloneDeriving extension:
-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- System imports:
import Control.Monad (unless, when)
import Data.List (intercalate)
import Data.Maybe (isNothing, fromJust)
-- import Debug.Trace (traceShow)
import System.Console.GetOpt
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (getArgs, getProgName, lookupEnv, setEnv)
import System.Exit (exitSuccess, exitFailure)
import System.Random (randomRIO)
import System.FileLock (lockFile, tryLockFile, unlockFile, SharedExclusive(..))

-- Local imports:
import Database
import PrettyPrint
import Build

-- Make using trace easier, for debugging only:
-- traceShow' arg = traceShow arg arg

-- We want to be able to show create process:
-- deriving instance Show CreateProcess
-- deriving instance Show StdStream
-- deriving instance Show CmdSpec

-- Print the program version and license information:
printVersion :: IO ()
printVersion = do putStrLn "Redo 0.1\nThe MIT License (MIT)\nCopyright (c) 2015"
                  exitSuccess

-- Print the program's help details:
printHelp :: String -> [OptDescr a] -> [String] -> IO b
printHelp programName opts errs = if null errs then do putStrLn $ helpStr opts 
                                                       exitSuccess
                                               else ioError (userError (concat errs ++ helpStr opts))
  where helpStr = usageInfo $ "Usage: " ++ programName ++ " [OPTION...] targets..."

-- Returns true if program was invoked from within a .do file, false if run from commandline
isRunFromDoFile :: IO Bool
isRunFromDoFile = do 
  -- This is the top-level (first) call to redo by if REDO_TARGET does not yet exist.
  redoTarget <- lookupEnv "REDO_TARGET"  
  if isNothing redoTarget || null (fromJust redoTarget) then return False else return True

-- Define program options:
-- The arguments to Option are:
-- 1) list of short option characters
-- 2) list of long option strings (without "--")
-- 3) argument descriptor
-- 4) explanation of option for user
data Flag = Version | Help | DashX | DashV | KeepGoing | Jobs | Shuffle deriving (Eq,Ord,Enum,Show,Bounded) 
options :: [OptDescr Flag]
options =
  [ Option ['j']         ["jobs"]          (NoArg Jobs)          "maximum number of parallel jobs to build at once (not yet supported!)"
  , Option ['h','H']     ["help"]          (NoArg Help)          "show usage details"
  , Option ['x']         ["xtrace"]        (NoArg DashX)         "print commands as they are executed with variables expanded"
  , Option ['v']         ["verbose"]       (NoArg DashV)         "print commands as they are read from .do files"
  , Option ['V','?']     ["version"]       (NoArg Version)       "print the version number"
  , Option ['k']         ["keep-going"]    (NoArg KeepGoing)     "keep building even if some targets fail"
  , Option ['s']         ["shuffle"]       (NoArg Shuffle)       "randomize the build order to find dependency bugs"
  ]

-- Helper function to get parse through commandline arguments and return options:
getOptions :: [String] -> IO ([Flag], [String])
getOptions argv = 
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> do programName <- getProgName
                     printHelp programName options errs 

-- Main function:
main :: IO ()
main = do 
  args <- getArgs
  progName <- getProgName

  -- Parse options, getting a list of option actions:
  opts <- getOptions args
  let (flags, targets) = opts 
  -- Show help or version information if asked:
  when (Version `elem` flags) printVersion
  when (Help `elem` flags) (printHelp progName options [])
  when (KeepGoing `elem` flags) (setEnv "REDO_KEEP_GOING" "TRUE")
  when (Shuffle `elem` flags) (setEnv "REDO_SHUFFLE" "TRUE")
  -- If there are shell args, set an environment variable that can be used by all
  -- redo calls after this.
  let shellArgs = intercalate "" [if DashX `elem` flags then "x" else "",
                                  if DashV `elem` flags then "v" else ""]
  unless (null shellArgs) (setEnv "REDO_SHELL_ARGS" shellArgs)
  -- Set the redo path variable to the current directory for the first call:
  redoInitPath <- lookupEnv "REDO_INIT_PATH"         -- Path where redo was initially invoked
  when (isNothing redoInitPath || null (fromJust redoInitPath)) (setEnv "REDO_INIT_PATH" =<< getCurrentDirectory) 

  -- Run the main:
  mainToRun' <- mainToRun
  mainToRun' progName =<< targetsToRun targets
  where
    -- Shuffle the targets if required:
    targetsToRun targets = do shuffleTargets <- lookupEnv "REDO_SHUFFLE"
                              if isNothing shuffleTargets then return targets 
                              else shuffle targets
    -- Check if redo is being run from inside of a .do file, or if this is the top level run
    -- Run the correct main accordingly
    mainToRun = do runFromDoFile <- isRunFromDoFile
                   return $ if runFromDoFile then mainDo else mainTop

-- The main function for redo run at a top level (outside of a .do file)
mainTop :: String -> [FilePath] -> IO()
mainTop progName targets = do
  -- Perform the proper action based on the program name:
  case progName of 
    -- Run redo only on buildable files from the target's directory
    "redo" -> do checkTargets targets'
                 lockFileMap (runActionIfBuildable redo) targets'
    -- Run redo-ifchange only on buildable files from the target's directory
    "redo-ifchange" -> do checkTargets targets
                          lockFileMap (runActionIfBuildable redoIfChange) targets
    -- redo-ifcreate and redo-always should only be run inside of a .do file
    "redo-ifcreate" -> runOutsideDoError progName 
    "redo-always" -> runOutsideDoError progName 
    _ -> return ()
  where
    -- If just 'redo' is run, then assume the default target as 'all'
    targets' = if null targets then ["all"] else targets
    -- Filter out targets that are not buildable:
    checkTargets :: [FilePath] -> IO ()
    checkTargets ts = mapM_ check ts
      where
        check target = do 
          -- If the user is trying to build a source file, then exit with an error
          -- else, continue to run the action on the target
          isSource <- isSourceFile target
          if isSource then do 
            putWarningStrLn $ "Warning: '" ++ target ++ "' exists and is marked as a source file. Not redoing."
            putWarningStrLn $ "If you believe '" ++ target ++ "' is buildable, remove it and try again."
            exitFailure
          else return ()

    -- This applies a function to a target if the target is not marked as a source file:
    runActionIfBuildable :: (FilePath -> IO ()) -> FilePath -> IO ()
    runActionIfBuildable action target = do 
      -- If the user is trying to build a source file, then exit with an error
      -- else, continue to run the action on the target
      isSource <- isSourceFile target
      if isSource then do 
        putWarningStrLn $ "Warning: '" ++ target ++ "' exists and is marked as a source file. Not redoing."
        putWarningStrLn $ "If you believe '" ++ target ++ "' is buildable, remove it and try again."
        exitFailure
      else do
       action target
       return ()
   
    -- Print warning message if redo-always or redo-ifcreate are run outside of a .do file
    runOutsideDoError :: String -> IO ()
    runOutsideDoError program = putWarningStrLn $ "Warning: '" ++ program ++ "' can only be invoked inside of a .do file."

-- The main function for redo run within a .do file
mainDo :: String -> [FilePath] -> IO()
mainDo progName targets = do
  -- Perform the proper action based on the program name:
  case progName of 
    -- Run redo only on buildable files from the target's directory
    "redo" -> lockFileMap redo targets 
    -- Run redo-ifchange only on buildable files from the target's directory
    -- Next store hash information for the parent target from the parent target's directory (current directory)
    "redo-ifchange" -> do lockFileMap redoIfChange targets
                          storeIfChangeDependencies targets
    -- Store redo-ifcreate dependencies for each target in the parent target's directory
    "redo-ifcreate" -> storeIfCreateDependencies targets
    -- Store a redo-always dependency for the parent target in the parent target's directory
    "redo-always" -> storeAlwaysDependency
    _ -> return ()

-- Randomly shuffle the order of a list:
-- http://en.literateprograms.org/Fisher-Yates_shuffle_(Haskell)
shuffle :: [a] -> IO [a]
shuffle lst = shuffle' lst []
  where
    shuffle' [] acc = return acc
    shuffle' l acc =
      do k <- randomRIO (0, length l - 1)
         let (lead, x:xs) = splitAt k l
         shuffle' (lead ++ xs) (x:acc)

-- Just run the do file for a 'redo' command:
redo :: FilePath -> IO ()
redo target = build noDoFileError target

-- Only run the do file if the target is not up to date for 'redo-ifchange' command:
redoIfChange :: FilePath -> IO ()
redoIfChange target = do 
  upToDate' <- upToDate target 
  -- Try to run redo if out of date, if it fails, print an error message:
  unless upToDate' $ build missingDo target
  where missingDo t = do exists <- doesFileExist t
                         unless exists $ noDoFileError t

-- Lock a file and run a function that takes that file as input.
-- If the file is already locked, skip running the function on that
-- file initially, and continue to trying to run the function on the next
-- file in the list. In a second pass, wait as long as it takes to lock 
-- on the files that were initially skipped before running the function.
-- This function allows us to build all the targets that don't have any 
-- lock contention first, buying us a little time before we wait to build
-- the files under lock contention.

-- ISSUE with this... lockFileName is the problem. We can only get it if
-- target is valid... and if target isn't valid, we skip a lot of the checks
-- that are built in downstream... We really need to lock the file after we know
-- that the file is valid, has a .do, isn't source, etc. Need to figure out how
-- to achieve that.
lockFileMap :: (FilePath -> IO ()) -> [FilePath] -> IO ()
lockFileMap func files = do
  remainingFiles <- mapM tryFunc files -- Try to lock file and build it, accumulate list of unbuilt files
  mapM_ waitFunc remainingFiles        -- Wait to acquire the lock, and build the remaining unbuilt files
  where
    tryFunc :: FilePath -> IO ((FilePath, FilePath))
    tryFunc file = do lckFileName <- lockFileName file 
                      case lckFileName of Nothing -> return ("", "")
                                          Just _ -> maybe (return (file, fromJust lckFileName)) (runFunc file) 
                                            =<< tryLockFile (fromJust lckFileName) Exclusive
    runFunc file lock = do func file
                           unlockFile lock
                           return ("", "")
    waitFunc :: (FilePath, FilePath) -> IO ()
    waitFunc ("", "") = return ()
    waitFunc (file, lckFileName) = do lock <- lockFile lckFileName Exclusive 
                                      func file
                                      unlockFile lock
