-- adding StandAloneDeriving extension:
-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- System imports:
import Control.Monad (unless, when)
import Control.Exception (catch, SomeException(..))
import Data.List (intercalate)
import Data.Map.Lazy (adjust, insert, fromList, toList)
import Data.Maybe (isNothing, fromJust, fromMaybe)
-- import Debug.Trace (traceShow)
import System.Console.GetOpt
import System.Directory (getModificationTime, makeAbsolute, renameFile, renameDirectory, removeFile, doesFileExist, getCurrentDirectory, setCurrentDirectory, doesDirectoryExist)
import System.Environment (getArgs, getEnvironment, getProgName, lookupEnv, setEnv)
import System.Exit (ExitCode(..), exitWith, exitSuccess, exitFailure)
import System.FilePath (dropExtension, takeExtensions, takeFileName, (</>), makeRelative, dropExtensions)
import System.IO (withFile, IOMode(..), hFileSize, hGetLine)
import System.Process (createProcess, waitForProcess, shell, CreateProcess(..))
import Data.Bool (bool)
import Data.Time (UTCTime(..), Day( ModifiedJulianDay ), secondsToDiffTime)
import System.Random (randomRIO)

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
    "redo" -> mapM_ redo targets 
    -- Run redo-ifchange only on buildable files from the target's directory
    -- Next store hash information for the parent target from the parent target's directory (current directory)
    "redo-ifchange" -> do mapM_ redoIfChange targets
                          mapM_ (performActionInDir (fromJust parentRedoPath) $ storeIfChangeDep $ fromJust parentRedoTarget) targetsRel2Parent
    -- Store redo-ifcreate dependencies for each target in the parent target's directory
    "redo-ifcreate" -> mapM_ (performActionInDir (fromJust parentRedoPath) $ storeIfCreateDep $ fromJust parentRedoTarget) targetsRel2Parent 
    -- Store a redo-always dependency for the parent target in the parent target's directory
    "redo-always" -> performActionInDir (fromJust parentRedoPath) storeAlwaysDep $ fromJust parentRedoTarget
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

-- Just run the do file for a 'redo' command:
redo :: FilePath -> IO ()
redo target = maybe (noDoFileError target) (runDoFileInDoDir target) =<< findDoFile target

-- Only run the do file if the target is not up to date for 'redo-ifchange' command:
redoIfChange :: FilePath -> IO ()
redoIfChange target = do 
  upToDate' <- upToDate target 
  -- Try to run redo if out of date, if it fails, print an error message:
  unless upToDate' $ maybe missingDo (runDoFileInDoDir target) =<< findDoFile target
  where missingDo = do exists <- doesFileExist target
                       unless exists $ noDoFileError target

-- Run a do file in the do file directory on the given target:
runDoFileInDoDir :: FilePath -> FilePath -> IO ()
runDoFileInDoDir target doFile = do
  (doFileDir, doFileName, targetRel2Do) <- getTargetRel2Do target doFile 
  performActionInDir doFileDir (runDoFile targetRel2Do) doFileName

-- Run the do script. Note: this must be run in the do file's directory!:
runDoFile :: FilePath -> FilePath -> IO () 
runDoFile target doFile = do 
  -- Get some environment variables:
  keepGoing' <- lookupEnv "REDO_KEEP_GOING"           -- Variable to tell redo to keep going even on failure
  shuffleDeps' <- lookupEnv "REDO_SHUFFLE"            -- Variable to tell redo to shuffle build order
  redoDepth' <- lookupEnv "REDO_DEPTH"                -- Depth of recursion for this call to redo
  shellArgs' <- lookupEnv "REDO_SHELL_ARGS"           -- Shell args passed to initial invokation of redo
  redoInitPath' <- lookupEnv "REDO_INIT_PATH"         -- Path where redo was initially invoked
  redoPath <- getCurrentDirectory                     -- Current redo path
  let redoInitPath = fromJust redoInitPath'           -- this should always be set from the first run of redo
  let redoDepth = show $ if isNothing redoDepth' then 0 else (read (fromJust redoDepth') :: Int) + 1
  let shellArgs = fromMaybe "" shellArgs'
  let keepGoing = fromMaybe "" keepGoing'
  let shuffleDeps = fromMaybe "" shuffleDeps'
  cmd <- shellCmd shellArgs doFile target

  -- Print what we are currently "redoing"
  absoluteTargetPath <- makeAbsolute target
  --putErrorStrLn $ absoluteTargetPath
  --putErrorStrLn $ redoInitPath
  putRedoStatus (read redoDepth :: Int) (makeRelative redoInitPath absoluteTargetPath)
  unless(null shellArgs) (putUnformattedStrLn $ "* " ++ cmd)

  -- Create the meta deps dir:
  createMetaDepsDir target

  -- Write out .do script as dependency:
  storeIfChangeDep target doFile

  -- Get the last time the target was modified:
  targetModTime <- getTargetModificationTime
   
  -- Add REDO_TARGET to environment, and make sure there is only one REDO_TARGET in the environment
  oldEnv <- getEnvironment
  let newEnv = toList $ adjust (++ ":.") "PATH" 
                      $ insert "REDO_PATH" redoPath
                      $ insert "REDO_KEEP_GOING" keepGoing
                      $ insert "REDO_SHUFFLE" shuffleDeps
                      $ insert "REDO_DEPTH" redoDepth
                      $ insert "REDO_INIT_PATH" redoInitPath 
                      $ insert "REDO_TARGET" target 
                      $ insert "REDO_SHELL_ARGS" shellArgs 
                      $ fromList oldEnv
  (_, _, _, processHandle) <- createProcess $ (shell cmd) {env = Just newEnv}
  exit <- waitForProcess processHandle
  case exit of  
    ExitSuccess -> moveTempFiles targetModTime
    ExitFailure code -> if null keepGoing
                        then redoError code $ nonZeroExitStr code
                        else putErrorStrLn $ nonZeroExitStr code
  -- Remove the temporary files:
  removeTempFiles target
  where
    nonZeroExitStr code = "Error: Redo script '" ++ doFile ++ "' exited with non-zero exit code: " ++ show code
    -- Temporary file names:
    tmp3 = tmp3File target 
    tmpStdout = tmpStdoutFile target 
    renameFileOrDir old new = catch(renameFile old new) 
      (\(_ :: SomeException) -> catch(renameDirectory old new) (\(_ :: SomeException) -> return ()))
    moveTempFiles prevTimestamp = do 
      tmp3Exists <- doesTargetExist tmp3
      stdoutExists <- doesTargetExist tmpStdout
      if tmp3Exists then
        whenTargetNotModified prevTimestamp (do
          renameFileOrDir tmp3 target
          when stdoutExists (do
            size <- fileSize tmpStdout
            when (size > 0) wroteToStdoutError ) )
      else if stdoutExists then
        whenTargetNotModified prevTimestamp (do
          size <- fileSize tmpStdout
          -- The else statement is a bit confusing, and is used to be compatible with apenwarr's implementation
          -- Basically, if the stdout temp file has a size of zero, we should remove the target, because no
          -- target should be created. This is our way of denoting the file as correctly build! 
          -- Usually this target won't exist anyways, but it might exist in the case
          -- of a modified .do file that was generating something, and now is not! In this case we remove the 
          -- old target to denote that the new .do file is working as intended. See the unit test "silencetest.do"
          if size > 0 then renameFileOrDir tmpStdout target 
                      -- a stdout file size of 0 was created. This is the default
                      -- behavior on some systems for ">"-ing a file that generatetes
                      -- no stdout. In this case, lets not clutter the directory, and
                      -- instead store a phony target in the meta directory
                      else do safeRemoveFile target
                              storePhonyTarget target)
      -- Neither temp file was created. This must be a phony target. Let's create it in the meta directory.
      else storePhonyTarget target
    -- TODO: This timestamp is only accurate to seconds. This would work much more consistantly if the time was
    -- accurate to sub seconds
    getTargetModificationTime :: IO UTCTime
    getTargetModificationTime = do
      targetExists <- doesTargetExist target
      if targetExists then getModificationTime target else return $ UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
    whenTargetNotModified :: UTCTime -> IO () -> IO ()
    whenTargetNotModified prevTimestamp action = do
        -- Allow user to create a directory directly. This is a special case allowed by python redo, and 
        -- we will allow it too. It is useful for do files that make a directory and populate it with the same name
        -- as the do file.
        dirExist <- doesDirectoryExist target 
        timestamp <- getTargetModificationTime
        --putWarningStrLn $ show timestamp ++ " > " ++ show prevTimestamp
        if not dirExist && timestamp > prevTimestamp then targetModifiedError else action
    wroteToStdoutError :: IO ()
    wroteToStdoutError  = redoError 1 $ "Error: '" ++ doFile ++ "' wrote to stdout and created $3.\n" ++
                                        "You should write status messages to stderr, not stdout." 
    targetModifiedError :: IO ()
    targetModifiedError = redoError 1 $ "Error: '" ++ doFile ++ "' modified '" ++ target ++ "' directly.\n" ++
                                        "You should update $3 (the temporary file) or stdout, not $1." 
    redoError :: Int -> String -> IO ()
    redoError code message = do putErrorStrLn message
                                removeTempFiles target
                                exitWith $ ExitFailure code

-- Pass redo script 3 arguments:
-- $1 - the target name
-- $2 - the target basename
-- $3 - the temporary target name
shellCmd :: String -> FilePath -> FilePath -> IO String
shellCmd shellArgs doFile target = do
  shebang <- readShebang doFile
  return $ unwords [shebang, show doFile, show target, show arg2, show $ tmp3File target, ">", show $ tmpStdoutFile target]
  where
    -- The second argument $2 is a tricky one. Traditionally, $2 is supposed to be the target name with the extension removed.
    -- What exactly constitutes the "extension" of a file can be debated. After much grudging... this implementation is now 
    -- compatible with the python implementation of redo. The value of arg2 depends on if the do file run is a default<.extensions>.do 
    -- file or a <targetName>.do file. For example the $2 for "redo file.x.y.z" run from different .do files is shown below:
    --
    --    .do file name |         $2  | description 
    --    file.x.y.z.do | file.x.y.z  | we do not know what part of the file is the extension... so we leave the entire thing 
    -- default.x.y.z.do |       file  | we know .x.y.z is the extension, so remove it
    --   default.y.z.do |     file.x  | we know .y.z is the extension, so remove it 
    --     default.z.do |   file.x.y  | we know .z is the extension, so remove it
    --       default.do | file.x.y.z  | we do not know what part of the file is the extension... so we leave the entire thing
    --
    arg2 = if (dropExtensions . takeFileName) doFile == "default" then createArg2 target doExtensions else target
    doExtensions = (takeExtensions . dropExtension) doFile -- remove .do, then grab the rest of the extensions
    createArg2 fname extension = if null extension then fname else createArg2 (dropExtension fname) (dropExtension extension)
    -- Read the shebang from a file and use that as the command. This allows us to run redo files that are in a language
    -- other than shell, ie. python or perl
    readShebang :: FilePath -> IO String
    readShebang file = readFirstLine >>= extractShebang
      where 
        readFirstLine = catch (withFile file ReadMode hGetLine) (\(_ :: SomeException) -> return "")
        extractShebang shebang = if take 2 shebang == "#!" then return $ drop 2 shebang else return $ "sh -e" ++ shellArgs

-- Temporary file names. Note we make these in the current directory, regardless of the target directory,
-- because we don't know if the target directory even exists yet. We can't redirect output to a non-existant
-- file.
tmp3File :: FilePath -> FilePath
tmp3File target = takeFileName target ++ ".redo1.temp" -- this temp file gets passed as $3 and is written to by programs that do not print to stdout
tmpStdoutFile :: FilePath -> FilePath
tmpStdoutFile target = takeFileName target ++ ".redo2.temp" -- this temp file captures what gets written to stdout

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
