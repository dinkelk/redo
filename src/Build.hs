{-# LANGUAGE ScopedTypeVariables #-}

module Build(redo, redoIfChange, isRunFromDoFile, storeIfChangeDependencies, storeIfCreateDependencies,
             storeAlwaysDependency) where

-- System imports:
import Control.Applicative ((<$>))
import Control.Monad (unless)
import Control.Exception (catch, SomeException(..))
import Data.Either (rights, lefts, isRight)
import Data.Map.Lazy (adjust, insert, fromList, toList)
import Data.Maybe (isJust, isNothing, fromJust, fromMaybe)
import Data.Bool (bool)
import System.Directory (doesDirectoryExist, renameFile, renameDirectory, removeFile, getCurrentDirectory)
import System.Environment (getEnvironment, lookupEnv, getEnv)
import System.Exit (ExitCode(..))
import System.FileLock (lockFile, tryLockFile, unlockFile, SharedExclusive(..), FileLock)
import System.FilePath ((</>), takeDirectory, dropExtension, takeExtensions, takeFileName, dropExtensions)
import System.IO (withFile, IOMode(..), hFileSize, hGetLine)
import System.Process (createProcess, waitForProcess, shell, CreateProcess(..))
import System.Posix.Types (ProcessID)

-- Local imports:
import Types
import Database 
import JobServer
import UpToDate
import PrettyPrint
import FilePathUtil

-- Store dependencies for redo-ifchange:
storeIfChangeDependencies :: [Target] -> IO ()
storeIfChangeDependencies = storeDependencies storeIfChangeDep

-- Store dependencies for redo-ifcreate:
storeIfCreateDependencies :: [Target] -> IO ()
storeIfCreateDependencies = storeDependencies storeIfCreateDep

-- Store dependency for redo-always:
storeAlwaysDependency :: IO ()
storeAlwaysDependency = do 
  key <- Key <$> getEnv "REDO_KEY"
  storeAlwaysDep key

-- Store dependencies given a store action and a list of dependencies to store:
storeDependencies :: (Key -> Target -> IO ()) -> [Target] -> IO ()  
storeDependencies storeAction dependencies = do 
  key <- Key <$> getEnv "REDO_KEY"
  parentRedoPath <- getCurrentDirectory
  canonicalizedDeps <- mapM (canonicalize parentRedoPath) dependencies
  mapM_ (storeAction key) canonicalizedDeps
  where canonicalize path dep = do cpath <- canonicalizePath' $ path </> unTarget dep
                                   return $ Target cpath

-- Returns true if program was invoked from within a .do file, false if run from commandline
isRunFromDoFile :: IO Bool
isRunFromDoFile = do 
  -- This is the top-level (first) call to redo by if REDO_KEY does not yet exist.
  redoTarget <- lookupEnv "REDO_KEY"  
  if isNothing redoTarget || null (fromJust redoTarget) then return False else return True

-- Does the target file or directory exist on the filesystem?
-- Checks if a target file is a buildable target, or if it is a source file
isTargetSource :: Key -> Target -> IO Bool
isTargetSource key target = bool (return False) isTargetSource' =<< doesTargetExist target
  where isTargetSource' = do hasDB <- doesDatabaseExist key
                             markedSource <- isSource key
                             return $ not hasDB || markedSource

-- Run do file if the target was not modified by the user first.
isTargetModified :: Key -> Maybe Stamp -> IO Bool
isTargetModified key currentStamp = do 
  cachedStamp <- getStamp key
  return $ isJust currentStamp && cachedStamp /= currentStamp

-- Print warning for trying to build a source file:
targetSourceWarning :: Target -> IO ExitCode
targetSourceWarning target = do
  putWarningStrLn $ "Warning: '" ++ unTarget target ++ "' exists and is marked as a source file. Not redoing."
  putWarningStrLn $ "If you believe '" ++ unTarget target ++ "' is buildable, remove it and try again."
  return $ ExitFailure 1

-- Print warning telling user that they modified a target outside of redo:
targetModifiedWarning :: Target -> IO ExitCode
targetModifiedWarning target = do putWarningStrLn $ "Warning: '" ++ unTarget target ++ "' was modified outside of redo. Skipping..."
                                  putWarningStrLn $ "If you want to rebuild '" ++ unTarget target ++ "', remove it and try again."
                                  return ExitSuccess

-- Missing do error function:
noDoFileError :: Target -> IO ExitCode
noDoFileError target = do putErrorStrLn $ "Error: No .do file found to build target '" ++ unTarget target ++ "'"
                          return $ ExitFailure 1

-- Just run the do file for a 'redo' command:
redo :: [Target] -> IO ExitCode
redo = buildTargets redo'
  where redo' target = do 
          source <- isTargetSource key target
          currentStamp <- safeStampTarget target
          modified <- isTargetModified key currentStamp
          if source then targetSourceWarning target
          else if modified then targetModifiedWarning target
          else maybe (noDoFileError target) (runDoFile key tempKey target currentStamp) =<< findDoFile target
          where key = getKey target
                tempKey = getTempKey target

-- Only run the do file if the target is not up to date for 'redo-ifchange' command:
redoIfChange :: [Target] -> IO ExitCode
redoIfChange = buildTargets redoIfChange'
  where 
    redoIfChange' target = do 
      source <- isTargetSource key target
      runFromDo <- isRunFromDoFile
      case (source, runFromDo) of
        (True, False) -> targetSourceWarning target
        (True, True) -> do initializeSourceDatabase key target
                           return ExitSuccess
        (False, _) -> do
          currentStamp <- safeStampTarget target
          modified <- isTargetModified key currentStamp
          if modified then targetModifiedWarning target
          else do
            upToDate' <- upToDate key tempKey target
            -- Try to run redo if out of date, if it fails, print an error message:
            unless' upToDate' (maybe (missingDo key target) (runDoFile key tempKey target currentStamp) =<< findDoFile target)
      where key = getKey target
            tempKey = getTempKey target
    -- Custom unless which return ExitSuccess if the condition is met
    unless' condition action = if condition then return ExitSuccess else action
    -- If a do file is not found then return an error message, unless the file exists,
    -- in which case it is a source file and does not need to be rebuilt
    missingDo key target = do exists <- doesTargetExist target
                              if exists then do
                                initializeSourceDatabase key target
                                return ExitSuccess
                              else noDoFileError target

-- This function allows us to build all the targets that don't have
-- lock contention first, buying us a little time before we wait to build
-- the files under lock contention.
buildTargets :: (Target -> IO ExitCode) -> [Target] -> IO ExitCode
buildTargets buildFunc targets = do
  handle <- getJobServer
  keepGoing'' <- lookupEnv "REDO_KEEP_GOING" -- Variable to tell redo to keep going even on failure
  let keepGoing' = fromMaybe "" keepGoing''
  let keepGoing = not $ null keepGoing'

  -- Try to lock file and build all targets and accumulate list of unbuilt targets:
  results <- mapM1 keepGoing (tryBuild handle) targets 
  let (remainingTargets, processStatus) = unzip results
  let exitCodes = rights processStatus
  let processIDs = lefts processStatus

  -- Exit immediately if something failed:
  maybe (do
    -- Give up token while we wait on all jobs to complete:
    returnToken handle
    remainingExitCodes <- mapM2 keepGoing waitOnJob processIDs
    -- Get token again before we continue:
    getToken handle
    -- Exit immediately if something failed:
    maybe (do
      -- Wait to acquire the lock, and build the remaining unbuilt files
      finalExitCodes <- mapM2 keepGoing (waitBuild handle) remainingTargets
      -- Make sure we wait on all jobs and gather the exit codes before returning:
      returnExitCode finalExitCodes
      )
      return (getFailingExitCode remainingExitCodes)
    )
    return (getFailingExitCode exitCodes)
  where
    -- Try to build the target if the do file can be found and there is no lock contention:
    tryBuild :: JobServerHandle -> Target -> IO ((Target, FilePath), Either ProcessID ExitCode)
    tryBuild handle target = do 
      absTarget <- Target <$> canonicalizePath' (unTarget target)
      tryBuild' absTarget
      where
        tryBuild' :: Target -> IO ((Target, FilePath), Either ProcessID ExitCode)
        tryBuild' absTarget = do lckFileName <- getLockFile absTarget
                                 maybe (return ((absTarget , lckFileName), Right ExitSuccess)) (runBuild absTarget) 
                                   =<< tryLockFile lckFileName Exclusive
        runBuild :: Target -> FileLock -> IO ((Target, FilePath), Either ProcessID ExitCode)
        runBuild absTarget lock = do processReturn <- runJob handle $ buildFunc absTarget 
                                     unlockFile lock
                                     return ((Target "", ""), processReturn)
    -- Wait to build the target if the do file is given, regardless of lock contention:
    waitBuild :: JobServerHandle -> (Target, FilePath) -> IO ExitCode
    waitBuild _ (Target "", "") = return ExitSuccess
    waitBuild handle (target, lckFileName) = do 
      -- Return my token before blocking on the filepath. This allows another redo process to be run
      -- in the meantime.
      returnToken handle 
      -- Wait to acquire a lock:
      lock <- lockFile lckFileName Exclusive 
      -- Ok we have a lock, but we need to get a token first, so release the lock
      -- and get a token. If we don't do it in this order we could encounter deadlock.
      unlockFile lock
      getToken handle
      -- Try to grab the lock again. This should work most of the time, unless a process
      -- beats us to the lock right after we released it. If we don't get a lock, try again.
      -- If we get the lock, run the build function and return the exit code.
      maybe (waitBuild handle (target, lckFileName)) 
            waitBuild' =<< tryLockFile lckFileName Exclusive
      where waitBuild' lock = do code <- buildFunc target
                                 unlockFile lock
                                 return code

    -- Helper function for returning a failing code if it exists, otherwise return success
    returnExitCode :: [ExitCode] -> IO ExitCode
    returnExitCode [] = return ExitSuccess
    returnExitCode (code:codes) = if code /= ExitSuccess then return code else returnExitCode codes

    -- Helper function which returns the failing exit code if it exists, otherwise nothing
    getFailingExitCode :: [ExitCode] -> Maybe ExitCode 
    getFailingExitCode [] = Nothing
    getFailingExitCode (code:codes) = if code /= ExitSuccess then Just code else getFailingExitCode codes

    -- Special mapMs which exits early if it detects that an operation fails:
    mapM1 :: Bool -> (Target -> IO ((Target, FilePath), Either ProcessID ExitCode)) 
                  -> [Target] -> IO [((Target, FilePath), Either ProcessID ExitCode)]
    mapM1 keepGoing f = mapM''
      where 
        mapM'' [] = return [((Target "", ""), Right ExitSuccess)]
        mapM'' (x:xs) = do 
          (a, newExitCode) <- f x 
          if isRight newExitCode && fromRight newExitCode /= ExitSuccess then 
            if keepGoing then runNext (a, newExitCode)
            else return [(a, newExitCode)]
          else runNext (a, newExitCode)
          where runNext current = do next <- mapM'' xs
                                     return $ current : next
                fromRight :: Either a b -> b
                fromRight (Right r) = r
                fromRight (Left _) = undefined

    -- Special mapMs which exits early if it detects that an operation fails:
    mapM2 :: Bool -> (a -> IO ExitCode) -> [a] -> IO [ExitCode]
    mapM2 keepGoing f = mapM''
      where 
        mapM'' [] = return [ExitSuccess]
        mapM'' (x:xs) = do 
          newExitCode <- f x 
          if newExitCode /= ExitSuccess then 
            if keepGoing then runNext newExitCode
            else return [newExitCode]
          else runNext newExitCode
          where runNext current = do next <- mapM'' xs
                                     return $ current : next

-- Run the do script. Note: this must be run in the do file's directory!:
-- and the absolute target must be passed.
runDoFile :: Key -> TempKey -> Target -> Maybe Stamp -> DoFile -> IO ExitCode
runDoFile key tempKey target currentTimeStamp doFile = do 
  -- Get some environment variables:
  keepGoing' <- lookupEnv "REDO_KEEP_GOING"           -- Variable to tell redo to keep going even on failure
  shuffleDeps' <- lookupEnv "REDO_SHUFFLE"            -- Variable to tell redo to shuffle build order
  redoDepth' <- lookupEnv "REDO_DEPTH"                -- Depth of recursion for this call to redo
  shellArgs' <- lookupEnv "REDO_SHELL_ARGS"           -- Shell args passed to initial invokation of redo
  redoInitPath' <- lookupEnv "REDO_INIT_PATH"         -- Path where redo was initially invoked
  sessionNumber' <- lookupEnv "REDO_SESSION"          -- Unique number to define this session
  noColor' <- lookupEnv "REDO_NO_COLOR"                -- Disable color printed redo status
  let redoInitPath = fromJust redoInitPath'           -- this should always be set from the first run of redo
  let redoDepth = show $ if isNothing redoDepth' then 0 else (read (fromJust redoDepth') :: Int) + 1
  let shellArgs = fromMaybe "" shellArgs'
  let noColor = fromMaybe "" noColor'
  let keepGoing = fromMaybe "" keepGoing'
  let shuffleDeps = fromMaybe "" shuffleDeps'
  let sessionNumber = fromMaybe "" sessionNumber'
  let targetRel2Do = Target $ makeRelative' redoPath (unTarget target)
  cmd <- shellCmd shellArgs doFile targetRel2Do
  targetIsDirectory <- doesDirectoryExist $ unTarget target

  -- Print what we are currently "redoing"
  putRedoInfo target
  unless (null shellArgs) (putUnformattedStrLn $ "* " ++ cmd)

  -- Create the target database:
  initializeTargetDatabase key doFile

  -- Create the dofile database:
  let doFileKey = getKey $ Target $ unDoFile doFile
  initializeSourceDatabase doFileKey $ Target $ unDoFile doFile

  -- Add to environment, and make sure there is only one of each variable added:
  oldEnv <- getEnvironment
  let newEnv = toList $ adjust (++ ":.") "PATH" 
                      $ insert "REDO_SESSION" sessionNumber
                      $ insert "REDO_KEEP_GOING" keepGoing
                      $ insert "REDO_SHUFFLE" shuffleDeps
                      $ insert "REDO_NO_COLOR" noColor
                      $ insert "REDO_DEPTH" redoDepth
                      $ insert "REDO_INIT_PATH" redoInitPath 
                      $ insert "REDO_KEY" (keyToFilePath key)
                      $ insert "REDO_SHELL_ARGS" shellArgs 
                      $ fromList oldEnv
  (_, _, _, processHandle) <- createProcess $ (shell cmd) {env = Just newEnv, cwd = Just redoPath}
  exit <- waitForProcess processHandle
  case exit of  
    ExitSuccess -> do exitCode <- moveTempFiles targetIsDirectory 
                      -- If the target exists, then store the target stamp
                      maybe (nonZeroExitStr exitCode) stampBuiltTarget
                        =<< getBuiltTargetPath key target
                      bool (return $ ExitFailure exitCode) (return ExitSuccess) (exitCode == 0) 
    ExitFailure code -> do markDirty tempKey -- we failed to build this target, so mark it dirty
                           removeTempFiles
                           nonZeroExitStr code
                           return $ ExitFailure code
  where
    -- Print generic exit string:
    nonZeroExitStr code = putErrorStrLn $ "Error: Redo script '" ++ unDoFile doFile ++ "' failed to build '" ++ 
                                           unTarget target ++ "' with exit code: " ++ show code 

    -- Store the stamp of the built target and mark it as clean
    -- consider storing timestamps in different place, so that they dont get blown away with initialization of db
    stampBuiltTarget builtTarget = do
      stamp <- stampTarget builtTarget
      storeStamp key stamp
      markClean tempKey -- we just built this target, so we know it is clean now
      removeTempFiles

    -- Remove the temporary files created for a target:
    removeTempFiles :: IO ()
    removeTempFiles = do safeRemoveTempFile tmp3
                         safeRemoveTempFile tmpStdout
      
    -- Temporary file names:
    redoPath = takeDirectory $ unDoFile doFile
    tmp3 = tmp3File target 
    tmpStdout = tmpStdoutFile redoPath target 

    -- Move temp files to target after creation:
    moveTempFiles :: Bool -> IO Int
    moveTempFiles targetIsDirectory = do 
      tmp3Exists <- doesTargetExist $ Target tmp3
      stdoutExists <- doesTargetExist $ Target tmpStdout
      stdoutSize <- fileSize tmpStdout
      newTimeStamp <- safeStampTarget target
      targetIsStillDirectory <- doesDirectoryExist $ unTarget target
      -- See if the user modified $1 directly... we don't care if the user modified a directory target however
      if currentTimeStamp /= newTimeStamp && not targetIsDirectory && not targetIsStillDirectory then 
        dollarOneModifiedError >> return 1
      else 
        if tmp3Exists then do
          safeRenameFileOrDir tmp3 target
          if stdoutExists && stdoutSize > 0 then wroteToStdoutError >> return 1 else return 0
        else if stdoutExists then
          -- If the user actually wrote data to standard out or built a directory on $1 then we just try to rename the file,
          -- and we leave a directory written on $1 alone.
          if stdoutSize > 0 || targetIsStillDirectory then safeRenameFile tmpStdout target >> return 0
          -- The else statement is a bit confusing, and is used to be compatible with apenwarr's implementation
          -- Basically, if the stdout temp file has a size of zero, we should remove the target, because no
          -- target should be created. This is our way of denoting the file as correctly build! 
          -- Usually this target won't exist anyways, but it might exist in the case
          -- of a modified .do file that was generating something, and now is not! In this case we remove the 
          -- old target to denote that the new .do file is working as intended. See the unit test "silencetest.do"
          ------------------------------------------------------------------------------------------------------
          -- Note: in this case a stdout file size of 0 was created. This is the default
          -- behavior on some systems for ">"-ing a file that generatetes
          -- no stdout. In this case, lets not clutter the directory, and
          -- instead store a phony target in the meta directory
          else do safeRemoveTempFile $ unTarget target
                  storePhonyTarget key
                  return 0
        -- Neither temp file was created. This must be a phony target. Let's create it in the meta directory.
        else storePhonyTarget key >> return 0
      where safeRenameFile :: FilePath -> Target -> IO ()
            safeRenameFile old new = catch (renameFile old (unTarget new)) (\(_ :: SomeException) -> return ())
            safeRenameFileOrDir :: FilePath -> Target -> IO () 
            safeRenameFileOrDir old new = catch(renameFile old (unTarget new)) 
              -- we need to remove the directory because renameDirectory does not overwrite on all platforms
              (\(_ :: SomeException) -> catch(do safeRemoveDirectoryRecursive (unTarget new)                                                  
                                                 renameDirectory old (unTarget new) ) (\(_ :: SomeException) -> return ()))
    
    -- Error messages for improper use of redo:
    wroteToStdoutError :: IO ()
    wroteToStdoutError = putErrorStrLn $
      "Error: '" ++ unDoFile doFile ++ "' wrote to stdout and created $3.\n" ++
      "You should write status messages to stderr, not stdout." 
    dollarOneModifiedError :: IO ()
    dollarOneModifiedError = putErrorStrLn $ 
      "Error: '" ++ unDoFile doFile ++ "' modified '" ++ unTarget target ++ "' directly.\n" ++
      "You should update $3 (the temporary file) or stdout, not $1." 

-- Pass redo script 3 arguments:
-- $1 - the target name
-- $2 - the target basename
-- $3 - the temporary target name
shellCmd :: String -> DoFile -> Target -> IO String
shellCmd shellArgs doFile target = do
  shebang <- readShebang doFile
  return $ unwords [shebang, quote $ unDoFile doFile, quote $ unTarget target, quote arg2, quote $ tmp3File target, ">", quote tmpStdoutFile']
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
    tmpStdoutFile' = takeFileName (unTarget target) ++ ".redo2.temp" -- this temp file captures what gets written to stdout
    quote string = "\"" ++ string ++ "\""
    arg2 = if (dropExtensions . takeFileName . unDoFile) doFile == "default" then createArg2 (unTarget target) doExtensions else unTarget target
    doExtensions = (takeExtensions . dropExtension . unDoFile) doFile -- remove .do, then grab the rest of the extensions
    createArg2 fname extension = if null extension then fname else createArg2 (dropExtension fname) (dropExtension extension)
    -- Read the shebang from a file and use that as the command. This allows us to run redo files that are in a language
    -- other than shell, ie. python or perl
    readShebang :: DoFile -> IO String
    readShebang file = readFirstLine >>= extractShebang
      where 
        readFirstLine = catch (withFile (unDoFile file) ReadMode hGetLine) (\(_ :: SomeException) -> return "")
        extractShebang shebang = if take 2 shebang == "#!" then return $ drop 2 shebang else return $ "sh -e" ++ shellArgs

-- Temporary files:
tmp3File :: Target -> FilePath
tmp3File target = unTarget target ++ ".redo1.temp" -- this temp file gets passed as $3 and is written to by programs that do not print to stdout
-- Stdout file name. Note we make this in provided directory, regardless of the target directory,
-- because we don't know if the target directory even exists yet. We can't redirect output to a non-existant
-- file. Users of this function will provide which directory to put the temporary filename
tmpStdoutFile :: FilePath -> Target -> FilePath
tmpStdoutFile dir target = dir </> takeFileName(unTarget target) ++ ".redo2.temp" -- this temp file captures what gets written to stdout

-- Function to check if file exists, and if it does, remove it:
safeRemoveTempFile :: FilePath -> IO ()
safeRemoveTempFile file = catch (removeFile file) (\(_ :: SomeException) -> safeRemoveDirectoryRecursive file)

-- Get the file size of a file
fileSize :: FilePath -> IO Integer
fileSize path = withFile path ReadMode hFileSize
