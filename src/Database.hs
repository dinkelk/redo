{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database(createMetaDepsDir, isSourceFile, storeIfChangeDep, storeIfCreateDep, 
                storeAlwaysDep, upToDate, findDoFile, noDoFileError, getTargetRel2Do,
                doesTargetExist)  where

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
doesTargetExist target = (||) <$> doesFileExist target <*> doesDirectoryExist target

-- Checks if a target file is a buildable target, or if it is a source file
isSourceFile :: FilePath -> IO Bool
isSourceFile target = bool (return False) (not <$> hasDependencies target) =<< doesTargetExist target

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

-- Returns true if all dependencies are up-to-date, or target is a source file, false otherwise.
upToDate :: FilePath -> IO Bool
upToDate target =
  -- If the target does not exist, then it is obviously not up-to-date, otherwise check it's dependencies
  doesTargetExist target >>= bool (do return False) 
    -- If the target exists, but has no do file to build it, then it is a source file, and is up to date, so return true
    (maybe (do return True) depsUpToDate =<< findDoFile target)
  where 
    -- Does a target have tracked dependencies, or is it a source file? If so, are they up to date?
    depsUpToDate :: FilePath -> IO Bool
    depsUpToDate doFile = do
      -- If target has no dependencies, then it is a source file, and can't be built, so it's up-to-date
      -- otherwise check the target's dependencies to see if they are up to date.
      -- Note: A target has dependencies if it has a metaDepsDir.
      -- Note: A target is a source file if the metaDepsDir doesn't exist
      metaDepsDir <- depFileDir' target doFile
      doesDirectoryExist metaDepsDir >>= bool (return True) (do
        doFileDir <- getAbsoluteDirectory doFile
        depsUpToDate' metaDepsDir doFileDir =<< getDirectoryContents metaDepsDir) 
    -- Are a target's dependencies up to date?
    depsUpToDate' :: FilePath -> FilePath -> [FilePath] -> IO Bool
    depsUpToDate' metaDepsDir doFileDir depHashFiles =
      if anyAlwaysDeps depHashFiles then return False
      else do 
        -- redo-ifcreate - if one of those files was created, we need to return False immediately
        depCreated' <- or `liftM` mapM (depCreated .  unEscapeIfCreatePath) (ifCreateDeps depHashFiles)
        if depCreated' then return False
        -- redo-ifchange - check these files hashes against those stored to determine if they are up to date
        --                 then recursively check their dependencies to see if they are up to date
        else and `liftM` mapM (depChanged metaDepsDir doFileDir . unEscapeIfChangePath) (ifChangeDeps depHashFiles)
    -- Returns true if there are any "-always" dependencies present:
    anyAlwaysDeps = any (fileHasPrepend always_dependency_prepend) 
    -- Functions which filter a set of dependencies for only those made with "-ifchange" or "-ifcreate"
    ifChangeDeps = filter (fileHasPrepend ifchange_dependency_prepend)
    ifCreateDeps = filter (fileHasPrepend ifcreate_dependency_prepend)
    -- Check if dep file begins with certain prepend string
    fileHasPrepend depPrepend xs = take 2 xs == ['.'] ++ [depPrepend]
    -- Has a dependency been created
    depCreated :: FilePath -> IO Bool
    depCreated dep = id <$> doesTargetExist dep 
    -- Has a dependency changed or are its dependencies to date? This function provides recursion:
    depChanged :: FilePath -> FilePath -> FilePath -> IO Bool
    depChanged metaDepsDir doFileDir dep = catch
      (do let hashFile = ifChangeDepFile' metaDepsDir dep
          oldHash <- withFile hashFile ReadMode hGetLine
          newHash <- computeHash $ doFileDir </> dep
          -- If the dependency is not up-to-date, then return false
          -- If the dependency is up-to-date then recurse to see if it's dependencies are up-to-date
          if oldHash /= newHash then return False
          else upToDate $ doFileDir </> dep)
      -- Ignore "." and ".." directories, and return true, return false if file dep doesn't exist
      (\e -> return (ioeGetErrorType e == InappropriateType))

-- directory.
findDoFile :: FilePath -> IO (Maybe FilePath)
findDoFile target = bool (defaultDoPath targetDir) (return $ Just targetDo) =<< doesFileExist targetDo
  where
    (targetDir, targetName) = splitFileName target
    targetDo = target ++ ".do"
    -- Try to find matching .do file by checking directories upwards of "." until a suitable match is 
    -- found or "/" is reached.
    defaultDoPath :: FilePath -> IO (Maybe FilePath)
    defaultDoPath dir = do
      absPath' <- canonicalizePath dir
      let absPath = if last absPath' == pathSeparator then takeDirectory absPath' else absPath'
      doFile <- listToMaybe `liftM` filterM doesFileExist (candidates absPath)
      if isNothing doFile && not (isDrive absPath) then defaultDoPath $ takeDirectory absPath 
      else return doFile
    -- List the possible default.do file candidates relative to the given path:
    candidates path = map ((path </>) . (++ ".do")) (getDefaultDo $ "default" ++ takeExtensions targetName)
    -- Form all possible matching default.do files in order of preference:
    getDefaultDo :: FilePath -> [FilePath]
    getDefaultDo filename = filename : if smallfilename == filename then [] else getDefaultDo $ dropFirstExtension filename
      where smallfilename = dropExtension filename
            basefilename = dropExtensions filename
            dropFirstExtension fname = basefilename ++ takeExtensions (drop 1 (takeExtensions fname))

-- Missing do error function:
noDoFileError :: FilePath -> IO()
noDoFileError target = do putErrorStrLn $ "Error: No .do file found for target '" ++ target ++ "'"
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
                               putErrorStrLn $ "Error: Encountered problem writing '" ++ contents ++ "' to '" ++ cd </> file ++ "'."
                               exitFailure)

-- Creation of an empty dep file for redo-always and redo-ifcreate
-- note may need to make specific one for redoifcreate and redoalways
createEmptyDepFile :: FilePath -> IO ()
createEmptyDepFile file = writeDepFile file "." 

-- Form the hash directory where a target's dependency hashes will be stored given the target
depFileDir :: FilePath -> IO (Maybe FilePath)
depFileDir target = maybe (return Nothing) justDepFileDir =<< findDoFile target 
  where
    justDepFileDir doFile = Just <$> depFileDir' target doFile

-- Form the hash directory where a target's dependency hashes will be stored given the target
-- and the path to the do file
depFileDir' :: FilePath -> FilePath -> IO FilePath
depFileDir' target doFile = do
  (doFileDirectory, _, targetRel2Do) <- getTargetRel2Do target doFile 
  return $ constructDir doFileDirectory targetRel2Do 
  where
    constructDir :: FilePath -> FilePath -> FilePath
    constructDir doFileDir targetRel2Do = doFileDir </> metaDir </> escapeDependencyPath '_' targetRel2Do

-- Given the path to the target and do file relative to the current directory
-- return the absolute path to the do directory and the relative paths from the
-- do directory to the do file, and the do directory to the target file
getTargetRel2Do :: FilePath -> FilePath -> IO (FilePath, FilePath, FilePath)
getTargetRel2Do target doFile = do
  (doDir, doFileName) <- splitFileName <$> makeAbsolute doFile
  targetRel2Do <- makeRelative doDir <$> makeAbsolute target
  return (doDir, doFileName, targetRel2Do)

-- Returns the absolute directory of a file path relative to the current dir:
getAbsoluteDirectory :: FilePath -> IO FilePath
getAbsoluteDirectory file = takeDirectory <$> makeAbsolute file 

-- Form the hash file path for a target's dependency given the metaDepsDir and the dependency
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
ifChangeDepFile' :: FilePath -> FilePath -> FilePath
ifChangeDepFile' = depFile' escapeIfChangePath
--ifCreateDepFile' :: FilePath -> FilePath -> FilePath
--ifCreateDepFile' = depFile' escapeIfCreatePath
alwaysDepFile :: FilePath -> IO (Maybe FilePath)
alwaysDepFile target = maybe (return Nothing) f =<< depFileDir target 
  where f metaDepsDir = return $ Just $ metaDepsDir </> escapeAlwaysPath
