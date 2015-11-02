{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database(createMetaDepsDir, isSourceFile, storeIfChangeDep, storeIfCreateDep, storeAlwaysDep, upToDate)  where

import Control.Applicative ((<$>))
import Control.Monad (liftM, guard)
import Control.Exception (catch, catchJust, SomeException(..), IOException)
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.MD5 (md5)
import GHC.IO.Exception (IOErrorType(..))
import System.Directory (getModificationTime, doesFileExist, getDirectoryContents, removeDirectoryRecursive, createDirectoryIfMissing, getCurrentDirectory, doesDirectoryExist)
import System.Exit (exitFailure)
import System.FilePath ((</>), isPathSeparator, pathSeparator)
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
createMetaDepsDir target = do
  catchJust (guard . isDoesNotExistError)
            (removeDirectoryRecursive metaDepsDir)
            (\_ -> return())
  createDirectoryIfMissing True metaDepsDir 
  where
    metaDepsDir = depFileDir target

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
  (\(_ :: IOException) -> return False)
  where fileHasPrepend depPrepend xs = take 2 xs == ['.'] ++ [depPrepend]
        hashDir = depFileDir target
        depCreated :: FilePath -> IO Bool
        depCreated dep = id <$> doesTargetExist dep 
        depUpToDate :: FilePath -> IO Bool
        depUpToDate dep = catch
          (do let hashFile = ifChangeDepFile target dep
              oldHash <- withFile hashFile ReadMode hGetLine
              newHash <- computeHash dep
              -- If the dependency is not up-to-date, then return false
              -- If the dependency is up-to-date then recurse to see if it's dependencies are up-to-date
              let depIsUpToDate = oldHash == newHash
              if not depIsUpToDate then return False
              else upToDate dep)
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
-- unEscapeAlwaysPath :: FilePath
-- unEscapeAlwaysPath = unEscapeDependencyPath always_dependency_prepend "redo-always"

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
  if depExists then do
    hash <- computeHash dep
    writeDepFile (ifChangeDepFile target dep) hash
  else do
    putWarningStrLn $ "Warning: Could not store dependency for '" ++ target ++ "' on '" ++ dep ++ "' because '" ++ dep ++ "' does not exist."
    putWarningStrLn $ "Make sure the .do which builds '" ++ dep ++ "' produces an output called '" ++ dep ++ "'."
    return ()

storeIfCreateDep :: FilePath -> FilePath -> IO ()
storeIfCreateDep target dep = createEmptyDepFile $ ifCreateDepFile target dep
storeAlwaysDep :: FilePath -> IO ()
storeAlwaysDep target = createEmptyDepFile $ alwaysDepFile target

-- Calculate the hash of a file. If the file is a directory,
-- then return the timestamp instead.
computeHash :: FilePath -> IO String
computeHash file = do 
  isDir <- doesDirectoryExist file
  if isDir then do
    timestamp <- getModificationTime file
    return (show $ timestamp )
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
