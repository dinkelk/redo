{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database(createMetaDepsDir, isSourceFile, storeIfChangeDep, storeIfCreateDep, 
                storeAlwaysDep, upToDate, findDoFile, noDoFileError, getTargetRel2Do)  where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (liftM, guard, filterM)
import Control.Exception (catch, catchJust, SomeException(..), IOException)
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.MD5 (md5)
import Data.Bool (bool)
import Data.Maybe (isNothing, listToMaybe, fromJust, isJust)
import GHC.IO.Exception (IOErrorType(..))
import System.Directory (makeAbsolute, canonicalizePath, getModificationTime, doesFileExist, getDirectoryContents, removeDirectoryRecursive, createDirectoryIfMissing, getCurrentDirectory, doesDirectoryExist)
import System.Exit (exitFailure)
import System.FilePath (normalise, dropTrailingPathSeparator, makeRelative, splitFileName, (</>), takeDirectory, isDrive, takeExtensions, dropExtensions, dropExtension, isPathSeparator, pathSeparator)
import System.IO (withFile, hGetLine, IOMode(..))
import System.IO.Error (ioeGetErrorType, isDoesNotExistError)

import PrettyPrint

-- TODO: make sure all functions here work with pathToTarget... not just target

-- | Directory for storing and fetching data on dependencies of redo targets.
metaDir :: String 
metaDir = ".redo"

-- Create meta data folder for storing md5 hashes:
-- Note: this function also blows out the old directory, which is good news because we don't want old
-- dependencies hanging around if we are rebuilding a file.
createMetaDepsDir :: FilePath -> IO ()
createMetaDepsDir target = maybe (noDoFileError target) f =<< depFileDir target
  where f metaDepsDir = do
          catchJust (guard . isDoesNotExistError)
                    (removeDirectoryRecursive metaDepsDir)
                    (\_ -> return())
          createDirectoryIfMissing True metaDepsDir 

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
  if targetExists then fmap not (hasDependencies target) else return False

-- Check's if a target has dependencies stored already
hasDependencies :: FilePath -> IO Bool
hasDependencies target = maybe (return False) doesDirectoryExist =<< depFileDir target
  
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
-- TODO: depFileDir assumes that the deps for a given target are stored in the 
-- same directory as that target. This is untrue. The deps for a given target
-- should be stored in the same directory as the do file which made that target
-- which is more often than not the same directory as the target


-- TODO... this needs to work with pathToTarget
upToDate :: FilePath -> IO Bool
upToDate pathToTarget = catch
  (do exists <- doesTargetExist pathToTarget
      --putErrorStrLn $ "uptoDate?: " ++ pathToTarget
      -- If the target does not exist, then it is obviously not up-to-date, otherwise
      if not exists then return False
      else do
        -- If target has dependencies, then it is a source file, then it can't be built, so it's up-to-date
        -- A target has dependencies if it has a metaDepsDir and it exists.
        -- A target is a source file if we can't find a metaDepsDir or if it the found metaDepsDir doesn't exist
        doFilePath' <- findDoFile pathToTarget
        let doFilePath'' = maybe "" takeDirectory doFilePath'
        doFilePath <- makeAbsolute doFilePath''
        --putWarningStrLn $ "doFilePath: " ++ doFilePath
        metaDepsDir' <- depFileDir pathToTarget 
        if isNothing metaDepsDir' then return True
        else do
          let metaDepsDir = fromJust metaDepsDir' 
          metaDepsDirExists <- doesDirectoryExist metaDepsDir 
          if not metaDepsDirExists then return True
          else do
            -- Otherwise, we check the target's dependencies to see if they have changed
            -- If the target's dependencies have changed, then the target is not up-to-date
            depHashFiles <- getDirectoryContents metaDepsDir
            -- Now we need to split up the file types and do different actions for each type:
            -- redo-always - we need to return False immediately
            if any (fileHasPrepend always_dependency_prepend) depHashFiles then return False
            else do 
              -- redo-ifcreate - if one of those files was created, we need to return False immediately
              let ifCreateDeps = filter (fileHasPrepend ifcreate_dependency_prepend) depHashFiles
              depCreated' <- or `liftM` mapM (depCreated .  unEscapeIfCreatePath) ifCreateDeps
              if depCreated' then return False
              else do 
                -- redo-ifchange - check these files hashes against those stored to determine if they are up to date
                --                 then recursively check their dependencies to see if they are up to date
                let ifChangeDeps = filter (fileHasPrepend ifchange_dependency_prepend) depHashFiles
                and `liftM` mapM (depUpToDate metaDepsDir doFilePath . unEscapeIfChangePath) ifChangeDeps)
  (\(_ :: IOException) -> return False)
  where 
    fileHasPrepend depPrepend xs = take 2 xs == ['.'] ++ [depPrepend]
    depCreated :: FilePath -> IO Bool
    depCreated dep = id <$> doesTargetExist dep 
    depUpToDate :: FilePath -> FilePath -> FilePath -> IO Bool
    depUpToDate metaDepsDir doFilePath dep = catch
      (do --putWarningStrLn $ "metaDepsDir: " ++ metaDepsDir
          --putWarningStrLn $ "dep: " ++ dep
          let hashFile = ifChangeDepFile' metaDepsDir dep
          --putWarningStrLn $ "hashFile: " ++ hashFile
          oldHash <- withFile hashFile ReadMode hGetLine
          newHash <- computeHash $ doFilePath </> dep
          -- If the dependency is not up-to-date, then return false
          -- If the dependency is up-to-date then recurse to see if it's dependencies are up-to-date
          let depIsUpToDate = oldHash == newHash
          --putWarningStrLn $ "uptodate?: " ++ show depIsUpToDate
          if not depIsUpToDate then return False
          else upToDate $ doFilePath </> dep)
      -- Ignore "." and ".." directories, and return true, return false if file dep doesn't exist
      (\e -> return (ioeGetErrorType e == InappropriateType))

-- directory.
findDoFile :: FilePath -> IO (Maybe FilePath)
findDoFile pathToTarget = bool (defaultDoPath targetDir) (return $ Just targetDo) =<< doesFileExist targetDo
  where
    (targetDir, target) = splitFileName pathToTarget
    targetDo = pathToTarget ++ ".do"
    -- Try to find matching .do file by checking directories upwards of "." until a suitable match is 
    -- found or "/" is reached.
    defaultDoPath :: FilePath -> IO (Maybe FilePath)
    defaultDoPath dir = do
      absPath' <- canonicalizePath dir
      let absPath = if last absPath' == pathSeparator then takeDirectory absPath' else absPath'
      --putWarningStrLn $ "absPath: " ++ absPath
      doFile <- listToMaybe `liftM` filterM doesFileExist (candidates absPath)
      if isNothing doFile && not (isDrive absPath) then defaultDoPath $ takeDirectory absPath 
      else return doFile
    -- List the possible default.do file candidates relative to the given path:
    candidates path = map ((path </>) . (++ ".do")) (getDefaultDo $ "default" ++ takeExtensions target)
    -- Form all possible matching default.do files in order of preference:
    getDefaultDo :: FilePath -> [FilePath]
    getDefaultDo filename = filename : if smallfilename == filename then [] else getDefaultDo $ dropFirstExtension filename
      where smallfilename = dropExtension filename
            basefilename = dropExtensions filename
            dropFirstExtension fname = basefilename ++ takeExtensions (drop 1 (takeExtensions fname))

-- Missing do error function:
-- TODO: make error handling more elegant with a monad pattern
-- ie. https://www.fpcomplete.com/school/starting-with-haskell/basics-of-haskell/10_Error_Handling
noDoFileError :: FilePath -> IO()
noDoFileError target = do putErrorStrLn $ "No .do file found for target '" ++ target ++ "'"
                          exitFailure

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
-- unEscapeAlwaysPath :: FilePath
-- unEscapeAlwaysPath = unEscapeDependencyPath always_dependency_prepend "redo-always"

-- This is the same as running normalise, but it always removes the trailing path
-- separator, and it always keeps a "./" in front of things in the current directory
-- and always removes "./" in front of things not in the current directory.
-- we use this to ensure consistancy of naming convention
sanitizeFilePath :: FilePath -> FilePath
sanitizeFilePath filePath = normalise $ dir </> file
  where (dir, file) = splitFileName . dropTrailingPathSeparator . normalise $ filePath

-- TODO: sanitize the input and output of these functions using:
-- dropTrailingPathSeparatorkk
-- Takes a file path and replaces all </> with @
escapeDependencyPath :: Char -> FilePath -> FilePath
escapeDependencyPath dependency_prepend path = (['.'] ++ [dependency_prepend]) ++ concatMap repl path'
  where path' = sanitizeFilePath path
        repl seperator_replacement = [seperator_replacement] ++ [seperator_replacement_escape]
        repl c   = if isPathSeparator c then [seperator_replacement] else [c]

-- Reverses escapeFilePath
unEscapeDependencyPath :: Char -> FilePath -> FilePath
unEscapeDependencyPath dependency_prepend name = sanitizeFilePath path
  where 
    path = if take 2 name == (['.'] ++ [dependency_prepend]) then unEscape $ drop 2 name else name
    unEscape [] = []
    unEscape string = first : unEscape rest
      where
        (first, rest) = repl string
        repl [] = ('\0',"")
        repl (x:xs) = if x == seperator_replacement
                      then if head xs == seperator_replacement_escape
                           then (seperator_replacement, tail xs)
                           else (pathSeparator, xs)
                      else (x, xs)

-- If the dependency exists then store
storeIfChangeDep :: FilePath -> FilePath -> IO ()
storeIfChangeDep target dep = do
  -- TODO: Not sure about this behavior... if the dep does not exist, which means that the .do file did NOT
  -- generate an output, we do not compute the hash of the dep, because it does not exist. This essentially
  -- removes it as a dependency... but I am not sure what else to do. The other option is to throw an error
  -- here if the dep does not exist.
  depExists <- doesTargetExist dep
  theDepFile <- ifChangeDepFile target dep
  if depExists && isJust theDepFile then do
    hash <- computeHash dep
    writeDepFile (fromJust theDepFile ) hash
  else do
    putWarningStrLn $ "Warning: Could not store dependency for '" ++ target ++ "' on '" ++ dep ++ "' because '" ++ dep ++ "' does not exist."
    putWarningStrLn $ "Make sure the .do which builds '" ++ dep ++ "' produces an output called '" ++ dep ++ "'."
    return ()

storeIfCreateDep :: FilePath -> FilePath -> IO ()
storeIfCreateDep target dep = maybe (noDoFileError target) createEmptyDepFile =<< ifCreateDepFile target dep

storeAlwaysDep :: FilePath -> IO ()
storeAlwaysDep target = maybe (noDoFileError target) createEmptyDepFile =<< alwaysDepFile target

-- Calculate the hash of a file. If the file is a directory,
-- then return the timestamp instead.
computeHash :: FilePath -> IO String
computeHash file = do 
  isDir <- doesDirectoryExist file
  if isDir then do
    timestamp <- getModificationTime file
    return (show timestamp)
  else (show . md5) `liftM` BL.readFile file

-- Calculate the hash of a target's dependency and write it to the proper meta data location
-- If the dependency doesn't exist, do not store a hash
writeDepFile :: FilePath -> FilePath -> IO ()
writeDepFile file contents = catch
  ( writeFile file contents )
  (\(_ :: SomeException) -> do cd <- getCurrentDirectory 
                               putErrorStrLn $ "Internal redo error: Encountered problen writing '" ++ contents ++ "' to '" ++ cd </> file ++ "'."
                               exitFailure)

-- Creation of an empty dep file for redo-always and redo-ifcreate
-- note may need to make specific one for redoifcreate and redoalways
createEmptyDepFile :: FilePath -> IO ()
createEmptyDepFile file = writeDepFile file "." 

-- Form the hash directory where a target's dependency hashes will be stored given the target
depFileDir :: FilePath -> IO (Maybe FilePath)
depFileDir pathToTarget = maybe (return Nothing) depFileDir' =<< findDoFile pathToTarget 
  where
    depFileDir' :: FilePath -> IO (Maybe FilePath)
    depFileDir' pathToDoFile = do
      (doFileDirectory, _, targetRel2Do) <- getTargetRel2Do pathToTarget pathToDoFile 
      return $ Just $ constructDir doFileDirectory targetRel2Do 
    constructDir :: FilePath -> FilePath -> FilePath
    constructDir doFileDir targetRel2Do = doFileDir </> metaDir </> escapeDependencyPath '_' targetRel2Do

-- Given the path to the target and do file relative to the current directory
-- return the absolute path to the do directory and the relative paths from the
-- do directory to the do file, and the do directory to the target file
getTargetRel2Do :: FilePath -> FilePath -> IO (FilePath, FilePath, FilePath)
getTargetRel2Do pathToTarget pathToDoFile = do
  (doDir, doFile) <- splitFileName <$> makeAbsolute pathToDoFile
  targetRel2Do <- makeRelative doDir <$> makeAbsolute pathToTarget
  return (doDir, doFile, targetRel2Do)

-- Returns the absolute directory of a file path relative to the current dir:
getAbsoluteDirectory :: FilePath -> IO FilePath
getAbsoluteDirectory file = takeDirectory <$> makeAbsolute file 

depFile' :: (FilePath -> FilePath) -> FilePath -> FilePath -> FilePath
depFile' escapeFunc metaDepsDir dep = metaDepsDir </> escapeFunc dep 

-- Form the hash file path for a target's dependency given the current target and its dependency
depFile :: (FilePath -> FilePath) -> FilePath -> FilePath -> IO (Maybe FilePath)
depFile escapeFunc target dep = maybe (return Nothing) f =<< depFileDir target
  where f metaDepsDir = return $ Just $ depFile' escapeFunc metaDepsDir dep

-- Functions to get the dependency path for each file type
ifChangeDepFile :: FilePath -> FilePath -> IO (Maybe FilePath)
ifChangeDepFile = depFile escapeIfChangePath
ifCreateDepFile :: FilePath -> FilePath -> IO (Maybe FilePath)
ifCreateDepFile = depFile escapeIfCreatePath
-- TODO: rename all of these to have "get" in front of them
-- and the "'" should not have a "'"
ifChangeDepFile' :: FilePath -> FilePath -> FilePath
ifChangeDepFile' = depFile' escapeIfChangePath
--ifCreateDepFile' :: FilePath -> FilePath -> FilePath
--ifCreateDepFile' = depFile' escapeIfCreatePath
alwaysDepFile :: FilePath -> IO (Maybe FilePath)
alwaysDepFile target = maybe (return Nothing) f =<< depFileDir target
  where f metaDepsDir = return $ Just $ metaDepsDir </> escapeAlwaysPath
