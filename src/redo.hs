-- adding StandAloneDeriving extension:
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

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
import System.Directory (renameFile, removeFile, doesFileExist, getDirectoryContents, removeDirectoryRecursive, createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)
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
putRedoStatus :: Int -> String -> IO ()
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

  -- Get the arguments to redo, if there are none, and this is top level call, use the default target "all"
  -- This is the top-level (first) call to redo by if REDO_PATH does not yet exist.
  redoPath <- lookupEnv "REDO_PATH"  
  let targets2redo = if null targets && isNothing redoPath then ["all"] else targets 
  parentRedoTarget <- lookupEnv "REDO_TARGET"

  -- Perform the proper action based on the program name:
  case progName of 
    "redo" -> mapM_ (performActionInTargetDir redo) targets2redo 
    "redo-ifchange" -> do mapM_ (performActionInTargetDir redoIfchange) targets2redo
                          when (isJust parentRedoTarget) ( mapM_ (storeHash $ fromJust parentRedoTarget) targets )
    "redo-ifcreate" -> putWarningStrLn "Sorry, redo-ifcreate is not yet supported."
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

-- Missing do error function:
noDoFileError :: FilePath -> IO()
noDoFileError target = do putErrorStrLn $ "No .do file found for target '" ++ target ++ "'"
                          exitFailure

-- Just run the do file for a 'redo' command:
redo :: FilePath -> IO ()
redo target = maybe (noDoFileError target) (runDoFile target) =<< doPath target

-- Only run the do file if the target is not up to date for 'redo-ifchange' command:
redoIfchange :: FilePath -> IO ()
redoIfchange target = do upToDate' <- upToDate target 
                         -- Try to run redo if out of date, if it fails, print an error message:
                         unless upToDate' $ maybe missingDo (runDoFile target) =<< doPath target
  where missingDo = do exists <- doesFileExist target 
                       unless exists $ noDoFileError target

-- Run the do script:
runDoFile :: String -> FilePath -> IO () 
runDoFile target doFile = do 
  -- Print what we are currently "redoing"
  currentDir <- getCurrentDirectory
  redoPath' <- lookupEnv "REDO_PATH"  
  redoDepth' <- lookupEnv "REDO_DEPTH"
  shellArgs' <- lookupEnv "REDO_SHELL_ARGS"
  let redoPath = fromMaybe currentDir redoPath'
  let absoluteTargetPath = currentDir </> target
  let redoDepth = show $ (if isNothing redoDepth' then 0 else (read (fromJust redoDepth') :: Int)) + 1
  let shellArgs = fromMaybe "" shellArgs'

  --putErrorStrLn $ "redo path:            " ++ redoPath 
  --putErrorStrLn $ "absolute target path: " ++ absoluteTargetPath 
  putRedoStatus (read redoDepth :: Int) (makeRelative redoPath absoluteTargetPath)
  
  -- Create the meta deps dir:
  createMetaDepsDir target

  -- Write out .do script as dependency:
  storeHash target doFile

  -- Add REDO_TARGET to environment, and make sure there is only one REDO_TARGET in the environment
  oldEnv <- getEnvironment
  let newEnv = toList $ adjust (++ ":.") "PATH" 
                      $ insert "REDO_DEPTH" redoDepth
                      $ insert "REDO_PATH" redoPath 
                      $ insert "REDO_TARGET" target 
                      $ insert "REDO_SHELL_ARGS" shellArgs 
                      $ fromList oldEnv
  (_, _, _, processHandle) <- createProcess $ (shell $ shellCmd shellArgs doFile target) {env = Just newEnv}
  exit <- waitForProcess processHandle
  case exit of  
    ExitSuccess -> catch (renameFile tmp3 target) handler1
    ExitFailure code -> do putWarningStrLn $ "Redo script '" ++ doFile ++ "' exited with non-zero exit code: " ++ show code
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
createMetaDepsDir target = do
  catchJust (guard . isDoesNotExistError)
            (removeDirectoryRecursive metaDepsDir)
            (\_ -> return())
  createDirectoryIfMissing True metaDepsDir 
  where
    metaDepsDir = depHashDir target

-- Pass redo script 3 arguments:
-- $1 - the target name
-- $2 - the target basename
-- $3 - the temporary target name
shellCmd shellArgs doFile target = unwords ["sh -e" ++ shellArgs, 
                                             show doFile, show target, show $ dropExtensions target, show $ tmp3File target, 
                                             ">", show $ tmpStdoutFile target]

-- Temporary file names:
tmp3File target = target ++ ".redo1.tmp" -- this temp file gets passed as $3 and is written to by programs that do not print to stdout
tmpStdoutFile target = target ++ ".redo2.tmp" -- this temp file captures what gets written to stdout

-- Remove the temporary files created for a target:
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

-- Returns true if all dependencies are up-to-date, false otherwise.
upToDate :: FilePath -> IO Bool
upToDate target = catch
  -- If the target does not exist, return then it is not up-to-date
  -- If the target exists, see if it's dependencies have changed
  -- If the target's dependencies have changed, it is not up-to-date
  (do exists <- doesFileExist target 
      if exists then
        do depHashFiles <- getDirectoryContents hashDir
           let depFiles = filterDotFiles $ map unEscapseFilePath depHashFiles
           and `liftM` mapM depUpToDate depFiles
        else return False)
  (\(e :: IOException) -> return False)
  where hashDir = depHashDir target 
        filterDotFiles :: [FilePath] -> [FilePath]
        filterDotFiles = filter (\a -> a /= ".." && a /= ".")
        depUpToDate :: String -> IO Bool
        depUpToDate dep = catch
          (do let depFile = dep
              let hashFile = depHashFile target dep
              oldHash <- withFile hashFile ReadMode hGetLine
              newHash <- computeHash depFile
              doScript <- doPath depFile
              case doScript of
                Nothing -> return (oldHash == newHash)
                Just _ -> do upToDate' <- upToDate depFile
                             return $ (oldHash == newHash) && upToDate')
          -- Ignore "." and ".." directories, and return true, return false if file dep doesn't exist
          (\e -> return (ioeGetErrorType e == InappropriateType))

-- Some #defines used for creating escaped dependency filenames. We want to avoid /'s.
#define seperator_replacement '^'
#define dependency_prepend '@'
#define seperator_replacement_escape '@'
-- Takes a file path and replaces all </> with @
escapeFilePath :: FilePath -> String
escapeFilePath path = [dependency_prepend] ++ concatMap repl path
  where repl seperator_replacement = [seperator_replacement] ++ [seperator_replacement_escape]
        repl c   = if isPathSeparator c then [seperator_replacement] else [c]

-- Reverses escapeFilePath
unEscapseFilePath :: String -> String
unEscapseFilePath name = if head name == dependency_prepend then unEscape $ tail name else name
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

-- Calculate the hash of a file
computeHash :: FilePath -> IO String
computeHash file = (show . md5) `liftM` BL.readFile file

-- Calculate the hash of a target's dependency and write it to the proper meta data location
-- If the dependency doesn't exist, do not store a hash
storeHash :: String -> FilePath -> IO ()
storeHash target dep = catch
  ( do hash <- computeHash dep
       writeFile (depHashFile target dep) hash )
  (\(e :: SomeException) -> return ())

-- Form the hash directory where a target's dependency hashes will be stored given the target
depHashDir :: String -> FilePath 
depHashDir target = metaDir </> (".__" ++ target ++ "__")

-- Form the hash file path for a target's dependency given the current target and its dependency
depHashFile :: String -> FilePath -> FilePath
depHashFile target dep = depHashDir target </> escapeFilePath dep

fileSize :: FilePath -> IO Integer
fileSize path = withFile path ReadMode hFileSize
