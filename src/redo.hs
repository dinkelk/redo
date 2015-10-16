-- adding StandAloneDeriving extension:
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

import Control.Monad (filterM, liftM, unless, guard, when)
import Control.Exception (catch, catchJust, SomeException(..), IOException)
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.MD5 (md5)
import Data.List (concatMap)
import Data.Map.Lazy (adjust, insert, fromList, toList)
import Data.Maybe (listToMaybe)
import Debug.Trace (traceShow)
import GHC.IO.Exception (IOErrorType(..))
import System.Console.ANSI (hSetSGR, SGR(..), ConsoleLayer(..), Color(..), ColorIntensity(..))
import System.Directory (renameFile, removeFile, doesFileExist, getDirectoryContents, removeDirectoryRecursive, createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)
import System.Environment (getArgs, getEnvironment, getProgName, lookupEnv)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath (replaceBaseName, hasExtension, takeBaseName, (</>), splitFileName, isPathSeparator, pathSeparator)
import System.IO (hPutStrLn, stderr, withFile, hGetLine, IOMode(..), hFileSize)
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
putColorStrLn :: Color -> String -> IO ()
putColorStrLn color string = do hSetSGR stderr [SetColor Foreground Vivid color]
                                hPutStrLn stderr $ string
                                hSetSGR stderr [SetColor Foreground Vivid Yellow]

putInfoStrLn :: String -> IO ()
putInfoStrLn string = putColorStrLn Green string

putWarningStrLn :: String -> IO ()
putWarningStrLn string = putColorStrLn Yellow string

putErrorStrLn :: String -> IO ()
putErrorStrLn string = putColorStrLn Red string

main :: IO ()
main = do 
  mapM_ redo =<< getArgs
  progName <- getProgName
  redoTarget' <- lookupEnv "REDO_TARGET"
  -- if the program name is redo-ifchange, then update the dependency hashes:
  case (progName, redoTarget') of 
    ("redo-ifchange", Just redoTarget) -> mapM_ (storeHash redoTarget) =<< getArgs
    ("redo-ifchange", Nothing) -> error "Missing REDO_TARGET environment variable."
    _ -> return ()

redo :: FilePath -> IO ()
redo pathToTarget = do 
  topDir <- getCurrentDirectory
  -- putInfoStrLn $ "... redoing " ++ (topDir </> target)
  catch (setCurrentDirectory dir) (\(e :: SomeException) -> do 
    putErrorStrLn $ "No such directory " ++ topDir </> dir
    exitWith $ ExitFailure 1)
  upToDate' <- upToDate target
  -- Try to run redo if out of date, if it fails, print an error message:
  unless upToDate' $ maybe missingDo runDoFile =<< doPath target
  setCurrentDirectory topDir
  where
    (dir, target) = splitFileName pathToTarget
    -- Print missing do file error:
    missingDo = do
      -- TODO: we should not need the line below, we should just error. we just need to not error if the
      -- call is redo-ifchange, not redo
      exists <- doesFileExist target -- should be deleted for redo, and included for redo-ifchange
      unless exists $ error $ "No .do file found for target '" ++ pathToTarget ++ "'"
    -- Run the do script:
    runDoFile :: FilePath -> IO ()
    runDoFile doFile = do
      topDir <- getCurrentDirectory
      putInfoStrLn $ "redo " ++ (topDir </> target)
      -- Create meta data folder:
      catchJust (guard . isDoesNotExistError)
                (removeDirectoryRecursive metaDepsDir)
                (\_ -> return())
      createDirectoryIfMissing True metaDepsDir 
      -- Write out .do script as dependency:
      storeHash target doFile
      -- Add REDO_TARGET to environment, and make sure there is only one REDO_TARGET in the environment
      oldEnv <- getEnvironment
      let newEnv = toList $ adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv
      (_, _, _, processHandle) <- createProcess $ (shell $ command doFile) {env = Just newEnv}
      exit <- waitForProcess processHandle
      case exit of  
        ExitSuccess -> catch (renameFile tmp3 target) handler1
        ExitFailure code -> do putErrorStrLn $ "\n" ++ "Redo script exited with non-zero exit code: " ++ show code
                               safeRemoveFile tmp3
                               safeRemoveFile tmpStdout
                               exitWith $ ExitFailure code
      -- Remove the temporary files:
      safeRemoveFile tmp3
      safeRemoveFile tmpStdout
    -- Dependency meta data directory for storing md5 hashes 
    metaDepsDir = depHashDir target
    -- Temporary file names:
    tmp3 = target ++ ".redo1.tmp" -- this temp file gets passed as $3 and is written to by programs that do not print to stdout
    tmpStdout = target ++ ".redo2.tmp" -- this temp file captures what gets written to stdout
    -- Pass redo script 3 arguments:
    -- $1 - the target name
    -- $2 - the target basename
    -- $3 - the temporary target name
    command doFile = unwords ["sh -e", doFile, target, takeBaseName target, tmp3, ">", tmpStdout]
    -- If renaming the tmp3 fails, let's try renaming tmpStdout:
    handler1 :: SomeException -> IO ()
    handler1 ex = catch 
                  (do size <- fileSize tmpStdout
                      when (size > 0) $ renameFile tmpStdout target)
                  handler2
    -- Renaming totally failed, lets alert the user:
    handler2 :: SomeException -> IO ()
    handler2 ex = putErrorStrLn "Redo could not copy results from temporary file"

-- Function to check if file exists, and if it does, remove it:
safeRemoveFile :: FilePath -> IO ()
safeRemoveFile file = bool (return ()) (removeFile file) =<< doesFileExist file

-- Take file path of target and return file path of redo script:
doPath :: FilePath -> IO (Maybe FilePath)
doPath target = listToMaybe `liftM` filterM doesFileExist candidates
  where candidates =  (target ++ ".do") : if hasExtension target 
                                          then [replaceBaseName target "default" ++ ".do"] 
                                          else []

-- Returns true if all dependencies are up-to-date, false otherwise.
upToDate :: FilePath -> IO Bool
upToDate pathToTarget = catch
  -- If the target does not exist, return then it is not up-to-date
  -- If the target exists, see if it's dependencies have changed
  -- If the target's dependencies have changed, it is not up-to-date
  (do exists <- doesFileExist pathToTarget 
      if exists then
        do depHashFiles <- getDirectoryContents hashDir
           let depFiles = filterDotFiles $ map unEscapseFilePath depHashFiles
           and `liftM` mapM depUpToDate depFiles
        else return False)
  (\(e :: IOException) -> return False)
  where (dir, target) = splitFileName pathToTarget
        hashDir = dir </> depHashDir target 
        filterDotFiles :: [FilePath] -> [FilePath]
        filterDotFiles = filter (\a -> a /= ".." && a /= ".")
        depUpToDate :: String -> IO Bool
        depUpToDate dep = catch
          (do let depFile = dir </> dep
              let hashFile = dir </> depHashFile target dep
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
