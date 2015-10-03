-- adding StandAloneDeriving extension:
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (filterM, liftM, unless, guard)
import Control.Exception (catch, catchJust, SomeException(..), IOException)
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.MD5 (md5)
import Data.List (concatMap)
import Data.Map.Lazy (adjust, insert, fromList, toList)
import Data.Maybe (listToMaybe)
import Debug.Trace (traceShow)
import GHC.IO.Exception (IOErrorType(..))
import System.Directory (renameFile, removeFile, doesFileExist, getDirectoryContents, removeDirectoryRecursive, createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)
import System.Environment (getArgs, getEnvironment, getProgName, lookupEnv)
import System.Exit (ExitCode(..))
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

-- Some global defines:
metaDir = ".redo"

main :: IO ()
main = do 
  mapM_ redo =<< getArgs
  progName <- getProgName
  redoTarget' <- lookupEnv "REDO_TARGET"
  -- if the program name is redo-ifchange, then update the dependency hashes:
  case (progName, redoTarget') of
    ("redo-ifchange", Just redoTarget) -> mapM_ (writeMD5 redoTarget) =<< getArgs
    ("redo-ifchange", Nothing) -> error "Missing REDO_TARGET environment variable."
    _ -> return ()

redo :: FilePath -> IO ()
redo pathToTarget = do 
  -- hPutStrLn stderr $ "... redoing " ++ target
  topDir <- getCurrentDirectory
  setCurrentDirectory dir
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
      exists <- doesFileExist target
      unless exists $ error $ "No .do file found for target '" ++ pathToTarget ++ "'"
    -- Run the do script:
    runDoFile :: FilePath -> IO ()
    runDoFile doFile = do
      hPutStrLn stderr $ "redo " ++ pathToTarget
      -- Create meta data folder:
      catchJust (guard . isDoesNotExistError)
                (removeDirectoryRecursive metaDepsDir)
                (\_ -> return())
      createDirectoryIfMissing True metaDepsDir 
      -- Write out .do script as dependency:
      writeMD5 target doFile
      -- Add REDO_TARGET to environment, and make sure there is only one REDO_TARGET in the environment
      oldEnv <- getEnvironment
      let newEnv = toList $ adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv
      (_, _, _, processHandle) <- createProcess $ (shell $ command doFile) {env = Just newEnv}
      exit <- waitForProcess processHandle
      case exit of  
        ExitSuccess -> catch (renameFile tmp3 target) handler1
        ExitFailure code -> hPutStrLn stderr $ "\n" ++ "Redo script exited with non-zero exit code: " ++ show code
      -- Remove the temporary files:
      safeRemoveFile tmp3
      safeRemoveFile tmpStdout
    -- Dependency meta data directory for storing md5 hashes 
    metaDepsDir = metaDir </> target
    -- Temporary file names:
    tmp3 = target ++ ".redo1.tmp" -- this temp file gets passed as $3 and is written to by programs that do not print to stdout
    tmpStdout = target ++ ".redo2.tmp" -- this temp file captures what gets written to stdout
    -- Pass redo script 3 arguments:
    -- $1 - the target name
    -- $2 - the target basename
    -- $3 - the temporary target name
    command doFile = unwords ["sh", doFile, target, takeBaseName target, tmp3, ">", tmpStdout]
    -- If renaming the tmp3 fails, let's try renaming tmpStdout:
    handler1 :: SomeException -> IO ()
    handler1 ex = catch 
                  (do size <- fileSize tmpStdout
                      if (size > 0) then
                        renameFile tmpStdout target 
                      else return ())
                  handler2
    -- Renaming totally failed, lets alert the user:
    handler2 :: SomeException -> IO ()
    handler2 ex = hPutStrLn stderr "Redo could not copy results from temporary file"

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
        do depHashFiles <- getDirectoryContents $ depHashDir
           let depFiles = filterDotFiles $ map unEscapseFilePath depHashFiles
           and `liftM` mapM depUpToDate depFiles
        else return False)
  (\(e :: IOException) -> return False)
  where (dir, target) = splitFileName pathToTarget
        depHashDir = dir </> metaDir </> target
        filterDotFiles :: [FilePath] -> [FilePath]
        filterDotFiles fileList = filter (\a -> a /= ".." && a /= ".") fileList
        depUpToDate :: String -> IO Bool
        depUpToDate dep = catch
          (do let depFile = dir </> dep
              let depHashFile = depHashDir </> escapeFilePath dep
              oldMD5 <- withFile depHashFile ReadMode hGetLine
              newMD5 <- fileMD5 depFile
              doScript <- doPath depFile
              case doScript of
                Nothing -> return (oldMD5 == newMD5)
                Just _ -> do upToDate' <- upToDate depFile
                             return $ (oldMD5 == newMD5) && upToDate')
          -- Ignore "." and ".." directories, and return true, return false if file dep doesn't exist
          (\e -> do return (ioeGetErrorType e == InappropriateType))

-- Takes a file path and replaces all </> with @
escapeFilePath :: FilePath -> String
escapeFilePath path = "@" ++ concatMap repl path
  where repl '^' = "^@"
        repl c   = if isPathSeparator(c) then "^" else [c]

-- Reverses escapeFilePath
unEscapseFilePath :: String -> String
unEscapseFilePath name = if head name == '@' then unEscape $ tail name else name
  where 
    unEscape [] = []
    unEscape string = first : unEscape rest
      where
        (first, rest) = repl string
        repl (x:xs) = if x == '^'
                      then if head xs == '@'
                           then ('^', tail xs)
                           else (pathSeparator, xs)
                      else (x, xs)

fileMD5 :: FilePath -> IO String
fileMD5 file = (show . md5) `liftM` BL.readFile file

writeMD5 :: String -> FilePath -> IO ()
writeMD5 target dep = do 
  hash <- fileMD5 dep
  writeFile (metaDir </> target </> escapeFilePath dep) hash

fileSize :: FilePath -> IO Integer
fileSize path = withFile path ReadMode hFileSize
