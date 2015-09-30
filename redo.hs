-- adding StandAloneDeriving extension:
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (filterM, liftM, unless, guard)
import Control.Exception (catch, catchJust, SomeException(..), IOException)
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.MD5 (md5) 
import Data.Map.Lazy (adjust, insert, fromList, toList)
import Data.Maybe (listToMaybe)
import Debug.Trace (traceShow)
import GHC.IO.Exception (IOErrorType(..))
import System.Directory (renameFile, removeFile, doesFileExist, getDirectoryContents, removeDirectoryRecursive, createDirectoryIfMissing)
import System.Environment (getArgs, getEnvironment, getProgName, lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath (replaceBaseName, hasExtension, takeBaseName, (</>))
import System.IO (hPutStrLn, stderr, withFile, hGetLine, IOMode(..))
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
--main = do
--  args <- getArgs
--  mapM_ redo args
main = do 
  mapM_ redo =<< getArgs
  progName <- getProgName
  redoTarget' <- lookupEnv "REDO_TARGET"
  -- if the program name is redo-ifchange, then update the dependency hashes:
  case (progName, redoTarget') of
    ("redo-ifchange", Just redoTarget) -> mapM_ (writeMD5 redoTarget) =<< getArgs
    ("redo-ifchange", Nothing) -> error "Missing REDO_TARGET environment variable."
    _ -> return ()
  where writeMD5 target dep = writeFile (metaDir </> target </> dep ) =<< md5File dep

redo :: String -> IO ()
redo target = do
  -- hPutStrLn stderr $ "... redoing " ++ target
  -- If the target file exists, check dependencies and redo if 
  -- the dependencies have changed. If the target file does not
  -- exist, redo (no need to check the dependencies)
  exists <- doesFileExist target
  if exists then 
    (do upToDate' <- upToDate metaDepsDir
        unless upToDate' $ redo' target)
    else redo' target
  where
    -- Try to run redo, if it fails, print an error message:
    redo' target = maybe missingDo runDoFile =<< redoPath target
    -- Print missing do file error:
    missingDo = do
      exists <- doesFileExist target
      unless exists $ error $ "No .do file found for target '" ++ target ++ "'"
    -- Run the do script:
    runDoFile :: FilePath -> IO ()
    runDoFile doFile = do
      hPutStrLn stderr $ "redo " ++ target
      -- Create meta data folder:
      catchJust (guard . isDoesNotExistError)
                (removeDirectoryRecursive metaDepsDir)
                (\_ -> return())
      createDirectoryIfMissing True metaDepsDir 
      -- Write out .do script as dependency:
      writeFile (metaDepsDir </> doFile) =<< md5File doFile
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
    tmp3 = target ++ ".redo-tmp" -- this temp file gets passed as $3 and is written to by programs that do not print to stdout
    tmpStdout = target ++ ".redo-tmp2" -- this temp file captures what gets written to stdout
    -- Pass redo script 3 arguments:
    -- $1 - the target name
    -- $2 - the target basename
    -- $3 - the temporary target name
    command doFile = unwords ["sh", doFile, target, takeBaseName target, tmp3, ">", tmpStdout]
    -- If renaming the tmp3 fails, let's try renaming tmpStdout:
    handler1 :: SomeException -> IO ()
    handler1 ex = catch (renameFile tmpStdout target) handler2
    -- Renaming totally failed, lets alert the user:
    handler2 :: SomeException -> IO ()
    handler2 ex = hPutStrLn stderr "Redo could not copy results from temporary file"

-- Function to check if file exists, and if it does, remove it:
safeRemoveFile :: FilePath -> IO ()
safeRemoveFile file = bool (return ()) (removeFile file) =<< doesFileExist file

-- Take file path of target and return file path of redo script:
redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = listToMaybe `liftM` filterM doesFileExist candidates
  where candidates =  (target ++ ".do") : if hasExtension target 
                                          then [replaceBaseName target "default" ++ ".do"] 
                                          else []

-- Returns true if all dependencies are up-to-date, false otherwise.
upToDate :: FilePath -> IO Bool
upToDate metaDepsDir = catch
  (do deps <- getDirectoryContents metaDepsDir 
      and `liftM` mapM depUpToDate deps)
  (\(e :: IOException) -> return False)
  where depUpToDate :: FilePath -> IO Bool
        depUpToDate dep = catch
          (do oldMD5 <- withFile (metaDepsDir </> dep) ReadMode hGetLine
              newMD5 <- md5File dep
              return ( oldMD5 == newMD5) )
          -- Ignore "." and ".." directories
          (\e -> return (ioeGetErrorType e == InappropriateType))

md5File :: FilePath -> IO String
md5File file = (show . md5) `liftM` BL.readFile file
