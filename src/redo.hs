-- adding StandAloneDeriving extension:
-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- System imports:
import Control.Monad (filterM, liftM, unless, when)
import Control.Exception (catch, SomeException(..))
import Data.List (intercalate)
import Data.Map.Lazy (adjust, insert, fromList, toList)
import Data.Maybe (isJust, listToMaybe, isNothing, fromJust, fromMaybe)
-- import Debug.Trace (traceShow)
import System.Console.GetOpt
import System.Directory (canonicalizePath, renameFile, removeFile, doesFileExist, getCurrentDirectory, setCurrentDirectory, makeAbsolute)
import System.Environment (getArgs, getEnvironment, getProgName, lookupEnv, setEnv)
import System.Exit (ExitCode(..), exitWith, exitSuccess, exitFailure)
import System.FilePath (pathSeparator, takeDirectory, isDrive, (</>), splitFileName, makeRelative, dropExtension, dropExtensions, takeExtensions)
import System.IO (withFile, IOMode(..), hFileSize)
import System.Process (createProcess, waitForProcess, shell, CreateProcess(..))
import Data.Bool (bool)

-- TODO: replace all currentDir </> path to use makeAbsolute
-- Local imports:
import Database
import PrettyPrint

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
data Flag = Version | Help | DashX | DashV deriving (Eq,Ord,Enum,Show,Bounded) 
options :: [OptDescr Flag]
options =
  [ Option ['V','?']     ["version"] (NoArg Version)       "show version number"
  , Option ['h','H']     ["help"]    (NoArg Help)          "show usage"
  , Option ['x']         ["sh-x"]    (NoArg DashX)         "run .do file using sh with -x option"
  , Option ['v']         ["sh-v"]    (NoArg DashV)         "run .do file using sh with -v option"
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
  -- If there are shell args, set an environment variable that can be used by all
  -- redo calls after this.
  let shellArgs = intercalate "" [if DashX `elem` flags then "x" else "",
                                  if DashV `elem` flags then "v" else ""]
  unless (null shellArgs) (setEnv "REDO_SHELL_ARGS" shellArgs)

  -- Check if redo is being run from inside of a .do file, or if this is the top level run
  -- Run the correct main accordingly
  runFromDoFile <- isRunFromDoFile
  let mainToRun = if runFromDoFile then mainDo else mainTop
  mainToRun progName targets

-- The main function for redo run at a top level (outside of a .do file)
mainTop :: String -> [FilePath] -> IO()
mainTop progName targets =
  -- Perform the proper action based on the program name:
  case progName of 
    -- Run redo only on buildable files from the target's directory
    "redo" -> mapM_ (runActionIfBuildable redo) targets'
    -- Run redo-ifchange only on buildable files from the target's directory
    "redo-ifchange" -> mapM_ (runActionIfBuildable redoIfChange) targets
    -- redo-ifcreate and redo-always should only be run inside of a .do file
    "redo-ifcreate" -> runOutsideDoError progName 
    "redo-always" -> runOutsideDoError progName 
    _ -> return ()
  where
    -- If just 'redo' is run, then assume the default target as 'all'
    targets' = if null targets then ["all"] else targets

    -- This applies a function to a target if the target is not marked as a source file:
    runActionIfBuildable :: (FilePath -> IO ()) -> FilePath -> IO ()
    runActionIfBuildable action target = do 
      -- If the user is trying to build a source file, then exit with an error
      -- else, continue to run the action on the target
      isSource <- isSourceFile target
      if isSource then do 
        putWarningStrLn $ target ++ ": exists and is marked as not buildable. Not redoing."
        putWarningStrLn $ "If you think this incorrect error, remove '" ++ target ++ "' and try again."
        exitFailure
      else do
       action target
       return ()
   
    -- Print warning message if redo-always or redo-ifcreate are run outside of a .do file
    runOutsideDoError :: String -> IO ()
    runOutsideDoError program = putWarningStrLn $ "'" ++ program ++ "' can only be invoked inside of a .do file."

-- The main function for redo run within a .do file
mainDo :: String -> [FilePath] -> IO()
mainDo progName targets = do
  parentRedoTarget <- lookupEnv "REDO_TARGET"
  parentRedoPath <- lookupEnv "REDO_PATH" -- directory where .do file was run from
  currentDir <- getCurrentDirectory
  -- All dependencies for the parent target should be stored in a .redo file in the
  -- parent target .do file invocation location.
  -- Note: All target listed here are relative to the current directory in the .do script. This could
  -- be different than the REDO_PATH variable, which represents the directory where the .do was invoked 
  -- if 'cd' was used in the .do script.
  -- So, let's get a list of targets relative to the parent .do file invocation location, REDO_PATH
  let targetsRel2Parent = map (makeRelative (fromJust parentRedoPath) . (currentDir </>)) targets
  -- Perform the proper action based on the program name:
  case progName of 
    -- Run redo only on buildable files from the target's directory
    "redo" -> mapM_ (redo) targets 
    -- Run redo-ifchange only on buildable files from the target's directory
    -- Next store hash information for the parent target from the parent target's directory (current directory)
    "redo-ifchange" -> do mapM_ (redoIfChange) targets
                          mapM_ (performActionInDir (fromJust parentRedoPath) $ storeIfChangeDep $ fromJust parentRedoTarget) targetsRel2Parent
    -- Store redo-ifcreate dependencies for each target in the parent target's directory
    "redo-ifcreate" -> mapM_ (performActionInDir (fromJust parentRedoPath) $ storeIfCreateDep $ fromJust parentRedoTarget) targetsRel2Parent 
    -- Store a redo-always dependency for the parent target in the parent target's directory
    "redo-always" -> performActionInDir (fromJust parentRedoPath) storeAlwaysDep $ fromJust parentRedoTarget
    _ -> return ()

-- This applies a function to a target in the directory provided and then
-- returns the current directory to the starting directory:
performActionInDir :: FilePath -> (FilePath -> IO ()) -> FilePath -> IO ()
performActionInDir dir action target = do
  topDir <- getCurrentDirectory
  --redoTarget' <- lookupEnv "REDO_TARGET"
  --case (redoTarget') of 
  --  (Just redoTarget) -> hPutStrLn stderr $ "... redoing " ++ redoTarget ++ "* -> " ++ (pathToTarget)
  --  (Nothing) -> hPutStrLn stderr $ "... redoing " ++ target ++ "  -> " ++ (pathToTarget)
  -- Hm, this is a bit weird. Need to support recursive upwards looking for default.do files.
  -- We cannot cd into a directory that does not exist... but we should be able to use default do to run something like
  -- redo dirThatWillSoonExist/theTarget. Currently, the cd into dirThatWillSoonExist to run .do will fail because it doesn't
  -- exist.
  -- In reality, we should first fine the correct .do file to run. Then we should cd to the directory of that .do file, then
  -- we should run the .do file. TODO!
  catch (setCurrentDirectory dir) (\(_ :: SomeException) -> do 
    putErrorStrLn $ "Error: No such directory " ++ topDir </> dir
    exitFailure)
  action target
  setCurrentDirectory topDir

-- This applies a function to a target in the directory that that target it located in
-- then it returns the current directory to the starting directory:
performActionInTargetDir :: (FilePath -> IO ()) -> FilePath -> IO ()
performActionInTargetDir action pathToTarget = performActionInDir dir action target
  where
    (dir, target) = splitFileName pathToTarget

-- This applies a function to a target in the directory that that target it located in
-- then it returns the current directory to the starting directory:
-- TODO: 1) checkout and debug the new default.do lookup function
--       2) make sure that performActionInDoFileDir works
--       3) Integrate this function with the existing logic for finding do files in redo and redoIfChange
performActionInDoFileDir :: (FilePath -> IO ()) -> FilePath -> IO ()
performActionInDoFileDir action pathToTarget = do
  doFile <- findDoFile pathToTarget 
  if isJust doFile then performActionInDir (takeDirectory $ fromJust doFile) action target
  else noDoFileError pathToTarget 
  where
    (dir, target) = splitFileName pathToTarget

-- Just run the do file for a 'redo' command:
redo :: FilePath -> IO ()
redo pathToTarget = do
  doFile' <- findDoFile pathToTarget 
  if isNothing doFile' then noDoFileError pathToTarget
  else do
    doFileAbsolute <- makeAbsolute $ fromJust doFile'
    let (doFileDir, doFile) = splitFileName doFileAbsolute
    targetFileAbsolute <- makeAbsolute pathToTarget
    let targetRel2Do = makeRelative doFileDir targetFileAbsolute
    putErrorStrLn $ "targetRel2Do: " ++ targetRel2Do
    putErrorStrLn $ "doFileDir: " ++ doFileDir
    performActionInDir (doFileDir) (runDoFile targetRel2Do) doFile
  where
    -- TODO, this is bad too. We need to get the name of the target from the DoFile's directory... need function to create this!
    (dir, target) = splitFileName pathToTarget

-- Only run the do file if the target is not up to date for 'redo-ifchange' command:
-- TODO: uptoDate needs to be run first, then perform action in target do. these need to be intermingled for epic success
redoIfChange :: FilePath -> IO ()
redoIfChange pathToTarget = do 
  -- hmmm prob need to be in the directory here?
  -- find do file for target, then go to that directory and see if it is up to date, if not, run the do file from that directory
  -- wait a minute... what if there is not a do file for the target?... because it is source. OHH NOES! Circular dep, figure out
  -- how to resolve.
  upToDate' <- upToDate pathToTarget 
  -- Try to run redo if out of date, if it fails, print an error message:
  unless upToDate' $ do
    doFile' <- findDoFile pathToTarget 
    if isNothing doFile' then missingDo
    else do
      doFileAbsolute <- makeAbsolute $ fromJust doFile'
      let (doFileDir, doFile) = splitFileName doFileAbsolute
      targetFileAbsolute <- makeAbsolute pathToTarget
      let targetRel2Do = makeRelative doFileDir targetFileAbsolute
      performActionInDir (doFileDir) (runDoFile targetRel2Do) doFile
  where
    missingDo = do exists <- doesFileExist pathToTarget
                   unless exists $ noDoFileError pathToTarget

-- Missing do error function:
noDoFileError :: FilePath -> IO()
noDoFileError target = do putErrorStrLn $ "No .do file found for target '" ++ target ++ "'"
                          exitFailure
-- Run the do script:
runDoFile :: FilePath -> FilePath -> IO () 
runDoFile target doFile = do 
  -- Print what we are currently "redoing"
  currentDir <- getCurrentDirectory
  redoInitPath' <- lookupEnv "REDO_INIT_PATH" -- Path where redo was initially invoked
  redoDepth' <- lookupEnv "REDO_DEPTH"        -- Depth of recursion for this call to redo
  shellArgs' <- lookupEnv "REDO_SHELL_ARGS"   -- Shell args passed to initial invokation of redo
  let redoPath = currentDir
  let redoInitPath = fromMaybe currentDir redoInitPath'
  let absoluteTargetPath = currentDir </> target
  let redoDepth = show $ (if isNothing redoDepth' then 0 else (read (fromJust redoDepth') :: Int)) + 1
  let shellArgs = fromMaybe "" shellArgs'

  --putErrorStrLn $ "redo path:            " ++ redoInitPath 
  --putErrorStrLn $ "absolute target path: " ++ absoluteTargetPath 
  putRedoStatus (read redoDepth :: Int) (makeRelative redoInitPath absoluteTargetPath)
  
  -- Create the meta deps dir:
  createMetaDepsDir target

  -- Write out .do script as dependency:
  storeIfChangeDep target doFile

  -- Add REDO_TARGET to environment, and make sure there is only one REDO_TARGET in the environment
  oldEnv <- getEnvironment
  let newEnv = toList $ adjust (++ ":.") "PATH" 
                      $ insert "REDO_PATH" redoPath
                      $ insert "REDO_DEPTH" redoDepth
                      $ insert "REDO_INIT_PATH" redoInitPath 
                      $ insert "REDO_TARGET" target 
                      $ insert "REDO_SHELL_ARGS" shellArgs 
                      $ fromList oldEnv
  (_, _, _, processHandle) <- createProcess $ (shell $ shellCmd shellArgs doFile target) {env = Just newEnv}
  exit <- waitForProcess processHandle
  case exit of  
    ExitSuccess -> catch (renameFile tmp3 target) handler1
    ExitFailure code -> do putErrorStrLn $ "Redo script '" ++ doFile ++ "' exited with non-zero exit code: " ++ show code
                           removeTempFiles target
                           exitWith $ ExitFailure code
  -- Remove the temporary files:
  removeTempFiles target
  where
    -- Temporary file names:
    tmp3 = tmp3File target 
    tmpStdout = tmpStdoutFile target 
    -- If renaming the tmp3 fails, let's try renaming tmpStdout:
    handler1 :: SomeException -> IO ()
    handler1 _ = catch 
                   (do size <- fileSize tmpStdout
                       -- The else statement is a bit confusing, and is used to be compatible with apenwarr's implementation
                       -- Basically, if the stdout temp file has a size of zero, we should remove the target, because no
                       -- target should be created. This is our way of denoting the file as correctly build! 
                       -- Usually this target won't exit anyways, but it might exist in the case
                       -- of a modified .do file that was generating something, and now is not! In this case we remove the 
                       -- old target to denote that the new .do file is working as intended. See the unit test "silencetest.do"
                       if size > 0 then renameFile tmpStdout target else safeRemoveFile target)
                   handler2
    -- Renaming totally failed, lets alert the user:
    handler2 :: SomeException -> IO ()
    handler2 _ = putErrorStrLn $ "Redo could not copy results from temporary file '" ++ tmpStdout ++ "'"

-- Pass redo script 3 arguments:
-- $1 - the target name
-- $2 - the target basename
-- $3 - the temporary target name
shellCmd :: String -> FilePath -> FilePath -> String
shellCmd shellArgs doFile target = unwords ["sh -e" ++ shellArgs, 
                                             show doFile, show target, show $ dropExtensions target, show $ tmp3File target, 
                                             ">", show $ tmpStdoutFile target]

-- Temporary file names:
tmp3File :: FilePath -> FilePath
tmp3File target = target ++ ".redo1.tmp" -- this temp file gets passed as $3 and is written to by programs that do not print to stdout
tmpStdoutFile :: FilePath -> FilePath
tmpStdoutFile target = target ++ ".redo2.tmp" -- this temp file captures what gets written to stdout

-- Remove the temporary files created for a target:
removeTempFiles :: FilePath -> IO ()
removeTempFiles target = do safeRemoveFile $ tmp3File target
                            safeRemoveFile $ tmpStdoutFile target
                     
-- Function to check if file exists, and if it does, remove it:
safeRemoveFile :: FilePath -> IO ()
safeRemoveFile file = bool (return ()) (removeFile file) =<< doesFileExist file

-- Get the file size of a file
fileSize :: FilePath -> IO Integer
fileSize path = withFile path ReadMode hFileSize
