-- adding StandAloneDeriving extension:
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

import Control.Applicative ((<$>))
import Control.Monad (filterM, liftM, unless, guard, when)
import Control.Exception (catch, catchJust, SomeException(..), IOException)
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.MD5 (md5)
import Data.List (concatMap, intercalate)
import Data.Map.Lazy (adjust, insert, fromList, toList)
import Data.Maybe (listToMaybe, isNothing, fromJust, isJust, fromMaybe)
import Debug.Trace (traceShow)
import GHC.IO.Exception (IOErrorType(..))
import System.Console.ANSI (hSetSGR, SGR(..), ConsoleLayer(..), Color(..), ColorIntensity(..), ConsoleIntensity(..))
import System.Console.GetOpt
import System.Directory (renameFile, removeFile, doesFileExist, getDirectoryContents, removeDirectoryRecursive, createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory, doesDirectoryExist)
import System.Environment (getArgs, getEnvironment, getProgName, lookupEnv, setEnv)
import System.Exit (ExitCode(..), exitWith, exitSuccess, exitFailure)
import System.FilePath (replaceBaseName, hasExtension, takeBaseName, (</>), splitFileName, isPathSeparator, pathSeparator, makeRelative, dropExtension, dropExtensions, takeExtensions)
import System.IO (hPutStrLn, hPutStr, stderr, stdout, withFile, hGetLine, IOMode(..), hFileSize)
import System.IO.Error (ioeGetErrorType, isDoesNotExistError)
import System.Process (createProcess, waitForProcess, shell, CreateProcess(..), StdStream(..), CmdSpec(..))
import Data.Bool (bool)

-- Make using trace easier, for debugging only:
traceShow' arg = traceShow arg arg

-- We want to be able to show create process:
deriving instance Show CreateProcess
deriving instance Show StdStream
deriving instance Show CmdSpec

-- | Directory for storing and fetching data on dependencies of redo targets.
metaDir = ".redo"

-- Set colors and write some text in those colors.
setConsoleDefault = hSetSGR stderr [Reset]
setConsoleColor color = hSetSGR stderr [SetColor Foreground Vivid color] 
setConsoleBold = hSetSGR stderr [SetConsoleIntensity BoldIntensity]
setConsoleFaint = hSetSGR stderr [SetConsoleIntensity FaintIntensity]
setConsoleColorDull color = hSetSGR stderr [SetColor Foreground Dull color] 

-- Put string to console in color:
putColorStrLn :: Color -> String -> IO ()
putColorStrLn color string = do setConsoleColor color
                                setConsoleBold 
                                hPutStrLn stderr string
                                setConsoleDefault 

-- Put info, warning, error strings to console:
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
                              hPutStr stderr $ "redo " ++ concat (replicate depth "  " )
                              setConsoleColor Green
                              setConsoleBold
                              hPutStrLn stderr file
                              setConsoleDefault

-- Print the program version and license information:
printVersion :: IO ()
printVersion = do putStrLn "Redo 0.1\nThe MIT License (MIT)\nCopyright (c) 2015"
                  exitSuccess

-- Print the program's help details:
printHelp :: String -> [OptDescr a] -> [String] -> IO b
printHelp programName options errs = if null errs then do putStrLn $ helpStr programName options 
                                                          exitSuccess
                                                  else ioError (userError (concat errs ++ helpStr programName options))
  where helpStr programName = usageInfo (header programName) 
        header progName = "Usage: " ++ progName ++ " [OPTION...] targets..."

-- Returns true if program was invoked from within a .do file, false if run from commandline
isRunFromDoFile :: IO Bool
isRunFromDoFile = do 
  -- This is the top-level (first) call to redo by if REDO_TARGET does not yet exist.
  redoTarget <- lookupEnv "REDO_TARGET"  
  if( isNothing redoTarget || null (fromJust redoTarget) ) then return False else return True

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
mainTop progName targets = do
  -- Perform the proper action based on the program name:
  case progName of 
    -- Run redo only on buildable files from the target's directory
    "redo" -> mapM_ (performActionInTargetDir $ runActionIfBuildable redo) targets'
    -- Run redo-ifchange only on buildable files from the target's directory
    "redo-ifchange" -> do mapM_ (performActionInTargetDir $ runActionIfBuildable redoIfChange) targets
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
  let targetsRel2Parent = map ((makeRelative $ fromJust parentRedoPath) . (currentDir </>)) targets
  -- Perform the proper action based on the program name:
  case progName of 
    -- Run redo only on buildable files from the target's directory
    "redo" -> mapM_ (performActionInTargetDir $ redo) targets 
    -- Run redo-ifchange only on buildable files from the target's directory
    -- Next store hash information for the parent target from the parent target's directory (current directory)
    "redo-ifchange" -> do mapM_ (performActionInTargetDir $ redoIfChange) targets
                          mapM_ (performActionInDir (fromJust parentRedoPath) $ storeIfChangeDep $ fromJust parentRedoTarget) targetsRel2Parent
    -- Store redo-ifcreate dependencies for each target in the parent target's directory
    "redo-ifcreate" -> mapM_ (performActionInDir (fromJust parentRedoPath) $ storeIfCreateDep $ fromJust parentRedoTarget) targetsRel2Parent 
    -- Store a redo-always dependency for the parent target in the parent target's directory
    "redo-always" -> performActionInDir (fromJust parentRedoPath) storeAlwaysDep $ fromJust parentRedoTarget
    _ -> return ()

-- This applies a function to a target in the directory that that target it located in
-- then it returns the current directory to the starting directory:
performActionInTargetDir :: (FilePath -> IO ()) -> FilePath -> IO ()
performActionInTargetDir action pathToTarget = do
  topDir <- getCurrentDirectory
  --redoTarget' <- lookupEnv "REDO_TARGET"
  --case (redoTarget') of 
  --  (Just redoTarget) -> hPutStrLn stderr $ "... redoing " ++ redoTarget ++ "* -> " ++ (pathToTarget)
  --  (Nothing) -> hPutStrLn stderr $ "... redoing " ++ target ++ "  -> " ++ (pathToTarget)
  catch (setCurrentDirectory dir) (\(e :: SomeException) -> do 
    putErrorStrLn $ "No such directory " ++ topDir </> dir
    exitFailure)
  action target
  setCurrentDirectory topDir
  where
    (dir, target) = splitFileName pathToTarget

-- This applies a function to a target in the directory that that target it located in
-- then it returns the current directory to the starting directory:
-- performActionInDir :: (FilePath -> IO ()) -> FilePath -> FilePath -> IO ()
performActionInDir dir action target = do
  topDir <- getCurrentDirectory
  --redoTarget' <- lookupEnv "REDO_TARGET"
  --case (redoTarget') of 
  --  (Just redoTarget) -> hPutStrLn stderr $ "... redoing " ++ redoTarget ++ "* -> " ++ (pathToTarget)
  --  (Nothing) -> hPutStrLn stderr $ "... redoing " ++ target ++ "  -> " ++ (pathToTarget)
  catch (setCurrentDirectory dir) (\(e :: SomeException) -> do 
    putErrorStrLn $ "No such directory " ++ topDir </> dir
    exitFailure)
  action target
  setCurrentDirectory topDir

-- Just run the do file for a 'redo' command:
redo :: FilePath -> IO ()
redo target = maybe (noDoFileError target) (runDoFile target) =<< doPath target

-- Only run the do file if the target is not up to date for 'redo-ifchange' command:
redoIfChange :: FilePath -> IO ()
redoIfChange target = do upToDate' <- upToDate target 
                         -- Try to run redo if out of date, if it fails, print an error message:
                         unless upToDate' $ maybe missingDo (runDoFile target) =<< doPath target
  where missingDo = do exists <- doesFileExist target 
                       unless exists $ noDoFileError target

-- Missing do error function:
noDoFileError :: FilePath -> IO()
noDoFileError target = do putErrorStrLn $ "No .do file found for target '" ++ target ++ "'"
                          exitFailure
-- Run the do script:
runDoFile :: FilePath -> FilePath -> IO () 
runDoFile target doFile = do 
  -- Print what we are currently "redoing"
  currentDir <- getCurrentDirectory
  redoPath' <- lookupEnv "REDO_PATH"          -- Path of current do file
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
    handler1 ex = catch 
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
    handler2 ex = putErrorStrLn $ "Redo could not copy results from temporary file '" ++ tmpStdout ++ "'"


-- Create meta data folder for storing md5 hashes:
createMetaDepsDir :: FilePath -> IO ()
createMetaDepsDir target = do
  catchJust (guard . isDoesNotExistError)
            (removeDirectoryRecursive metaDepsDir)
            (\_ -> return())
  createDirectoryIfMissing True metaDepsDir 
  where
    metaDepsDir = depFileDir target

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

-- Take file path of target and return file path of redo script:
doPath :: FilePath -> IO (Maybe FilePath)
doPath target = listToMaybe `liftM` filterM doesFileExist candidates
  where candidates = (target ++ ".do") : map (++ ".do") (getDefaultDo $ "default" ++ takeExtensions target)
        getDefaultDo :: FilePath -> [FilePath]
        getDefaultDo filename = filename : if smallfilename == filename then [] else getDefaultDo $ dropFirstExtension filename
          where smallfilename = dropExtension filename
                basefilename = dropExtensions filename
                dropFirstExtension filename = basefilename ++ takeExtensions (drop 1 (takeExtensions filename))

-- Does the target file or directory exist on the filesystem.
doesTargetExist :: FilePath -> IO Bool
doesTargetExist target = do 
  -- hPutStrLn stderr $ "Is " ++ target ++ " uptodate?"
  fileExists <- doesFileExist target 
  dirExists <- doesDirectoryExist target
  -- hPutStrLn stderr $ "file exists? " ++ show fileExists
  -- hPutStrLn stderr $ "dir exists? " ++ show dirExists
  return (fileExists || dirExists)

-- Checks if a target file is a buildable target, or if it is a source file
isSourceFile :: FilePath -> IO Bool
isSourceFile target = do
  targetExists <- doesTargetExist target
  if targetExists then hasDependencies target else return False

-- Check's if a target has dependencies stored already
hasDependencies :: FilePath -> IO Bool
hasDependencies target = do 
  hashDirExists <- doesDirectoryExist hashDir
  if hashDirExists then return False else return True
  where hashDir = depFileDir target
  
-- Some #defines used for creating escaped dependency filenames. We want to avoid /'s.
#define seperator_replacement '^'
#define seperator_replacement_escape '@'
-- We use different file prepends to denote different kinds of dependencies:
-- ~ redo-always
-- % redo-ifcreate
-- @ redo-ifchange
#define ifchange_dependency_prepend '@'
#define ifcreate_dependency_prepend '%'
#define always_dependency_prepend '~'

-- Returns true if all dependencies are up-to-date, false otherwise.
upToDate :: FilePath -> IO Bool
upToDate target = catch
  (do exists <- doesTargetExist target 
      -- If the target does not exist, then it is obviously not up-to-date, otherwise
      if not exists then return False
      else do
        -- If target has dependencies, then it is a source file, then it can't be built, so it's up-to-date
        isSource <- hasDependencies target
        if isSource then return True 
        else do 
          -- Otherwise, we check the target's dependencies to see if they have changed
          -- If the target's dependenvies have changed, then the target is not up-to-date
          depHashFiles <- getDirectoryContents hashDir
          -- Now we need to split up the file types and do different actions for each type:
          -- redo-always - we need to return False immediately
          if any (fileHasPrepend always_dependency_prepend) depHashFiles then return False
          else do 
            -- redo-ifcreate - if one of those files was created, we need to return False immediately
            let ifCreateDeps = filter (fileHasPrepend ifcreate_dependency_prepend) depHashFiles
            depCreated' <- or `liftM` mapM (depCreated . unEscapeIfCreatePath) ifCreateDeps
            if depCreated' then return False
            else do 
              -- redo-ifchange - check these files hashes against those stored to determine if they are up to date
              --                 then recursively check their dependencies to see if they are up to date
              let ifChangeDeps = filter (fileHasPrepend ifchange_dependency_prepend) depHashFiles
              and `liftM` mapM (depUpToDate . unEscapeIfChangePath) ifChangeDeps)
  (\(e :: IOException) -> return False)
  where fileHasPrepend depPrepend xs = take 2 xs == ['.'] ++ [depPrepend]
        hashDir = depFileDir target
        depCreated :: FilePath -> IO Bool
        depCreated dep = do id <$> doesFileExist dep 
        depUpToDate :: FilePath -> IO Bool
        depUpToDate dep = catch
          (do let depFile = dep
              let hashFile = ifChangeDepFile target dep
              oldHash <- withFile hashFile ReadMode hGetLine
              newHash <- computeHash depFile
              -- If the dependency is not up-to-date, then return false
              -- If the dependency is up-to-date then recurse to see if it's dependencies are up-to-date
              let depIsUpToDate = (oldHash == newHash)
              if not depIsUpToDate then return False
              else do upToDate' <- upToDate depFile
                      return upToDate')
          -- Ignore "." and ".." directories, and return true, return false if file dep doesn't exist
          (\e -> return (ioeGetErrorType e == InappropriateType))

-- Functions to escape and unescape dependencies of different types:
escapeIfChangePath :: FilePath -> FilePath 
escapeIfChangePath = escapeDependencyPath ifchange_dependency_prepend
unEscapeIfChangePath :: FilePath -> FilePath 
unEscapeIfChangePath = unEscapeDependencyPath ifchange_dependency_prepend

escapeIfCreatePath :: FilePath -> FilePath 
escapeIfCreatePath = escapeDependencyPath ifcreate_dependency_prepend
unEscapeIfCreatePath :: FilePath -> FilePath 
unEscapeIfCreatePath = unEscapeDependencyPath ifcreate_dependency_prepend

escapeAlwaysPath :: FilePath
escapeAlwaysPath = escapeDependencyPath always_dependency_prepend "redo-always"
unEscapeAlwaysPath :: FilePath
unEscapeAlwaysPath = unEscapeDependencyPath always_dependency_prepend "redo-always"

-- Takes a file path and replaces all </> with @
escapeDependencyPath :: Char -> FilePath -> FilePath
escapeDependencyPath dependency_prepend path = (['.'] ++ [dependency_prepend]) ++ concatMap repl path
  where repl seperator_replacement = [seperator_replacement] ++ [seperator_replacement_escape]
        repl c   = if isPathSeparator c then [seperator_replacement] else [c]

-- Reverses escapeFilePath
unEscapeDependencyPath :: Char -> FilePath -> FilePath
unEscapeDependencyPath dependency_prepend name = if take 2 name == (['.'] ++ [dependency_prepend]) then unEscape $ drop 2 name else name
  where 
    unEscape [] = []
    unEscape string = first : unEscape rest
      where
        (first, rest) = repl string
        repl (x:xs) = if x == seperator_replacement
                      then if head xs == seperator_replacement_escape
                           then (seperator_replacement, tail xs)
                           else (pathSeparator, xs)
                      else (x, xs)

storeIfChangeDep :: FilePath -> FilePath -> IO ()
storeIfChangeDep target dep = do 
  hash <- computeHash dep
  writeDepFile (ifChangeDepFile target dep) hash
storeIfCreateDep :: FilePath -> FilePath -> IO ()
storeIfCreateDep target dep = createEmptyDepFile $ ifCreateDepFile target dep
storeAlwaysDep :: FilePath -> IO ()
storeAlwaysDep target = createEmptyDepFile $ alwaysDepFile target

-- Calculate the hash of a file
computeHash :: FilePath -> IO String
computeHash file = (show . md5) `liftM` BL.readFile file

-- Calculate the hash of a target's dependency and write it to the proper meta data location
-- If the dependency doesn't exist, do not store a hash
writeDepFile :: FilePath -> FilePath -> IO ()
writeDepFile file contents = catch
  ( do writeFile file contents )
  (\(e :: SomeException) -> do cd <- getCurrentDirectory 
                               putErrorStrLn $ "Error writing '" ++ contents ++ "' to '" ++ cd </> file ++ "'. If this happens, there is a bug in redo :("
                               exitFailure)

-- Creation of an empty dep file for redo-always and redo-ifcreate
-- note may need to make specific one for redoifcreate and redoalways
createEmptyDepFile :: FilePath -> IO ()
createEmptyDepFile file = writeDepFile file "." 

-- Form the hash directory where a target's dependency hashes will be stored given the target
depFileDir :: FilePath -> FilePath 
depFileDir target = metaDir </> (".__" ++ target ++ "__")

-- Form the hash file path for a target's dependency given the current target and its dependency
depFile :: (FilePath -> FilePath) -> FilePath -> FilePath -> FilePath
depFile escapeFunc target dep = depFileDir target </> escapeFunc dep

-- Functions to get the dependency path for each file type
ifChangeDepFile :: FilePath -> FilePath -> FilePath
ifChangeDepFile = depFile escapeIfChangePath
ifCreateDepFile :: FilePath -> FilePath -> FilePath
ifCreateDepFile = depFile escapeIfCreatePath
alwaysDepFile :: FilePath -> FilePath
alwaysDepFile target = depFileDir target </> escapeAlwaysPath

-- Get the file size of a file
fileSize :: FilePath -> IO Integer
fileSize path = withFile path ReadMode hFileSize


-- TODO... remove deps directory if .do file has changed. helps with ifcreate that are deleted.
