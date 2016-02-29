{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MetaDirectory(clearCache, clearLockFiles, redoMetaDirectory, initializeDatabase, storeIfChangeDependencies, storeIfCreateDependencies, 
                     storeAlwaysDependency, storePhonyTarget, createLockFile, markTargetClean, 
                     markTargetDirty, storeStamp, metaDir, getTargetBuiltTimeStamp, ifChangeMetaFileToTarget,
                     ifCreateMetaFileToTarget, doesMetaDirExist, getBuiltTargetPath, isTargetMarkedDirty, 
                     isTargetMarkedClean, readMetaFile, getCachedDoFile, getMetaDirDependencies, removeMetaDir,
                     isSourceFile, MetaDir(..), LockFile(..), MetaFile(..), Key(..), getKey) where

import Control.Applicative ((<$>))
import Control.Exception (catch, SomeException(..))
import qualified Data.ByteString.Char8 as BS
import Crypto.Hash.MD5 (hash) 
import Data.Hex (hex)
import Data.Bool (bool)
import System.Directory (getAppUserDataDirectory, doesFileExist, getDirectoryContents, createDirectoryIfMissing, getCurrentDirectory, doesDirectoryExist)
import System.FilePath (normalise, dropTrailingPathSeparator, makeRelative, splitFileName, (</>), isPathSeparator, pathSeparator)
import System.Environment (getEnv)
import System.Exit (exitFailure)

import PrettyPrint
import Helpers
import Types

---------------------------------------------------------------------
-- Type Definitions:
---------------------------------------------------------------------
newtype Key = Key { keyToFilePath :: FilePath } deriving (Eq) -- The data base key for a target
newtype MetaDir = MetaDir { unMetaDir :: FilePath } deriving (Eq) -- The meta directory associated with a target
newtype MetaFile = MetaFile { unMetaFile :: FilePath } deriving (Eq) -- A meta file stored within a meta directory
newtype LockFile = LockFile { lockFileToFilePath :: FilePath } deriving (Eq) -- A lock file for synchronizing access to meta directories

---------------------------------------------------------------------
-- # Defines
---------------------------------------------------------------------
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

---------------------------------------------------------------------
-- Functions initializing the meta directory for a target
---------------------------------------------------------------------
-- Directory for storing and fetching data on dependencies of redo targets.
redoMetaDirectory :: IO FilePath
redoMetaDirectory = getAppUserDataDirectory "redo"

-- Directory for storing database entries for redo targets and sources
redoDatabaseDirectory :: IO FilePath
redoDatabaseDirectory = do
  root <- redoMetaDirectory
  return $ root </> "database"

-- Directory for storing single redo session cached information. This speeds up
-- the upToDate function
redoCacheDirectory :: IO FilePath
redoCacheDirectory = do
  root <- redoMetaDirectory
  return $ root </> "cache"

-- Directory for storing file locks to syncronize parallel builds
redoLockFileDirectory :: IO FilePath
redoLockFileDirectory = do
  root <- redoMetaDirectory
  return $ root </> "locks"

-- Create meta data folder for storing hashes and/or timestamps and return the folder name
-- We store a dependency for the target on the do file
-- Note: this function also blows out the old directory, which is good news because we don't want old
-- dependencies hanging around if we are rebuilding a file.
--initializeMetaDepsDir :: MetaDir -> DoFile -> IO ()
--initializeMetaDepsDir metaDepsDir doFile = do
--  removeMetaDir metaDepsDir
--  createMetaDir metaDepsDir
--  -- Write out .do script as dependency:
--  storeStampFile metaDepsDir (Target $ unDoFile doFile) (Target $ unDoFile doFile)
--  -- Cache the do file:
--  cacheDoFile metaDepsDir doFile
initializeDatabase :: Key -> MetaDir -> DoFile -> IO ()
initializeDatabase key metaDepsDir doFile = do
  clearDatabaseDirectory key
  createDatabaseDirectory key
  -- Write out .do script as dependency:
  storeStampFile metaDepsDir (Target $ unDoFile doFile) (Target $ unDoFile doFile)
  -- Cache the do file:
  cacheDoFile key doFile

---------------------------------------------------------------------
-- Functions clearing meta data information
---------------------------------------------------------------------
-- Clear a database entry
clearDatabaseDirectory :: Key -> IO ()
clearDatabaseDirectory key = safeRemoveDirectoryRecursive =<< getDatabaseDirectory key

-- Clear the entire redo cache directory:
clearCache :: IO ()
clearCache = safeRemoveDirectoryRecursive =<< redoCacheDirectory

-- Clear the entire redo cache directory:
clearLockFiles :: IO ()
clearLockFiles = safeRemoveDirectoryRecursive =<< redoLockFileDirectory

---------------------------------------------------------------------
-- Functions getting database keys for targets
---------------------------------------------------------------------
-- Get the database for a given target:
getKey :: Target -> IO Key
getKey target = do
  hashedTarget <- hashString target
  return $ Key $ pathify hashedTarget
  where 
    pathify "" = ""
    pathify string = x </> pathify xs
      where (x,xs) = splitAt 2 string

-- Create a hash string for a target:
-- TODO remove canonicalize Path here
hashString :: Target -> IO FilePath
hashString target = do 
  absPath <- canonicalizePath' $ unTarget target
  return $ hex $ BS.unpack $ hash $ BS.pack absPath

---------------------------------------------------------------------
-- Functions getting meta directories
---------------------------------------------------------------------
-- Get the database directory for a target:
getDatabaseDirectory :: Key -> IO FilePath
getDatabaseDirectory key = do
  databaseDirectory <- redoDatabaseDirectory
  return $ databaseDirectory </> keyToFilePath key

-- Create the database directory for a target:
createDatabaseDirectory :: Key -> IO ()
createDatabaseDirectory key = safeCreateDirectoryRecursive =<< getDatabaseDirectory key

-- Get the database directory for a target's ifchange dependencies:
getIfChangeDirectory :: Key -> IO FilePath
getIfChangeDirectory key = do
  dbDir <- getDatabaseDirectory key
  return $ dbDir </> "r"

-- Get the database directory for a target's ifcreate dependencies:
getIfCreateDirectory :: Key -> IO FilePath
getIfCreateDirectory key = do
  dbDir <- getDatabaseDirectory key
  return $ dbDir </> "c"

-- Get the database directory for a target's always dependencies:
getAlwaysDirectory :: Key -> IO FilePath
getAlwaysDirectory key = do
  dbDir <- getDatabaseDirectory key
  return $ dbDir </> "a"

-- Get the database directory for a target's always dependencies:
getStampDirectory :: Key -> IO FilePath
getStampDirectory key = do
  dbDir <- getDatabaseDirectory key
  return $ dbDir </> "s"

-- Get the database directory for a target's always dependencies:
getPhonyTargetDirectory :: Key -> IO FilePath
getPhonyTargetDirectory key = do
  dbDir <- getDatabaseDirectory key
  return $ dbDir </> "p"

-- Get the database directory for a target's always dependencies:
getDoFileDirectory :: Key -> IO FilePath
getDoFileDirectory key = do
  dbDir <- getDatabaseDirectory key
  return $ dbDir </> "d"

-- Get the cache directory for a target:
getCacheDirectory :: Key -> IO FilePath
getCacheDirectory key = do
  cacheDir <- redoCacheDirectory
  return $ cacheDir </> keyToFilePath key

-- Get the lock file directory for a target:
getLockFileDirectory :: Key -> IO FilePath
getLockFileDirectory key = do
  lockFileDir <- redoLockFileDirectory
  return $ lockFileDir </> keyToFilePath key

-- TODO delete
metaDir target = do
  key <- getKey target
  dir <- getDatabaseDirectory key
  return $ MetaDir $ dir
-- Form the hash directory where a target's dependency hashes will be stored given the target
--metaDir :: Target -> IO MetaDir
--metaDir target = do
--  metaRoot <- redoMetaDirectory 
--  hashedTarget <- hashString target
--  return $ MetaDir $ metaRoot </> pathify hashedTarget
--  where 
--    pathify "" = ""
--    pathify string = x </> pathify xs
--      where (x,xs) = splitAt 2 string

---------------------------------------------------------------------
-- Functions acting on MetaDir
---------------------------------------------------------------------
removeMetaDir :: MetaDir -> IO ()
removeMetaDir dir = safeRemoveDirectoryRecursive $ unMetaDir dir

createMetaDir :: MetaDir -> IO ()
createMetaDir dir = safeCreateDirectoryRecursive (unMetaDir dir)

getMetaDirContents :: MetaDir -> IO [MetaFile]
getMetaDirContents dir = do contents <- getDirectoryContents $ unMetaDir dir
                            return $ map MetaFile contents

-- Given a meta directory for a target return a tuple of dependency file lists
-- in the order ifchanged, ifcreated, always
getMetaDirDependencies :: MetaDir -> IO ([MetaFile], [MetaFile], [MetaFile])
getMetaDirDependencies dir = do
  depHashFiles <- getMetaDirContents dir
  return (ifChangeDeps depHashFiles, ifCreateDeps depHashFiles, ifAlwaysDeps depHashFiles)
  where 
    -- Functions which filter a set of dependencies for only those made with "-ifchange", "-ifcreate", or "always"
    ifAlwaysDeps = filter (fileHasPrepend always_dependency_prepend) 
    ifChangeDeps = filter (fileHasPrepend ifchange_dependency_prepend)
    ifCreateDeps = filter (fileHasPrepend ifcreate_dependency_prepend)
    fileHasPrepend depPrepend metaFile = take 2 (unMetaFile metaFile) == '.' : [depPrepend]

---------------------------------------------------------------------
-- Existance functions:
---------------------------------------------------------------------
doesMetaDirExist :: MetaDir -> IO Bool
doesMetaDirExist depDir = doesDirectoryExist $ unMetaDir depDir

-- TODO: make this only check directories since that is all we are storing now
doesMetaFileExist :: MetaFile -> IO Bool
doesMetaFileExist metaFile = bool (doesDirectoryExist mFile) (return True) =<< doesFileExist mFile
  where mFile = unMetaFile metaFile

---------------------------------------------------------------------
-- Functions writing meta files:
---------------------------------------------------------------------
-- Calculate the hash of a target's dependency and write it to the proper meta data location
-- If the dependency doesn't exist, do not store a hash
writeMetaFile :: MetaFile -> Stamp -> IO ()
writeMetaFile file contents = catch
  ( BS.writeFile fileToWrite byteContents )
  (\(_ :: SomeException) -> do cd <- getCurrentDirectory 
                               putErrorStrLn $ "Error: Encountered problem writing '" ++ BS.unpack byteContents ++ "' to '" ++ cd </> fileToWrite ++ "'."
                               exitFailure)
  where byteContents = unStamp contents
        fileToWrite = unMetaFile file

-- Get the stamp of a target and store it in the meta directory
storeStampFile :: MetaDir -> Target -> Target -> IO ()
storeStampFile metaDepsDir depName depToStamp = writeMetaFile theMetaFile =<< getStamp depToStamp
  where theMetaFile = ifChangeMetaFile metaDepsDir depName

-- Creation of an empty dep file:
createEmptyMetaFile :: MetaFile -> IO ()
--createEmptyMetaFile file = writeMetaFile file (Stamp $ BS.singleton '.')
-- Note: I am creating a directory instead because it is faster than writing a single byte to a file
--createEmptyMetaFile file = catch (createDirectory $ unMetaFile file) (\(_ :: SomeException) -> return ())
createEmptyMetaFile file = safeCreateDirectoryRecursive (unMetaFile file)

writeMetaFile' :: FilePath -> String -> IO ()
writeMetaFile' file contents = do 
  safeRemoveDirectoryRecursive file
  safeCreateDirectoryRecursive dir 
  where dir = file </> contents

readMetaFile' file = do
  contents <- getDirectoryContents file
  return $ contents !! 2

-- TODO append meta file

-- Write jibberish to a meta file so that it always compares bad:
storeBadMetaFile :: MetaFile -> IO ()
storeBadMetaFile file = writeMetaFile file (Stamp $ BS.singleton '.')

-- Store the stamp of a dependency if it exists. If it does not exist, then we store a blank stamp file
-- because the target still depenends on this target, it just failed to built last time, so we store a
-- blank (bad) stamp that will never match a successfully built target
storeIfChangeDep :: MetaDir -> Target -> IO ()
storeIfChangeDep metaDepsDir dep = maybe (storeBadMetaFile theMetaFile) (storeStampFile metaDepsDir dep) =<< getBuiltTargetPath' dep
  where theMetaFile = ifChangeMetaFile metaDepsDir dep

-- Store the ifcreate dep only if the target doesn't exist right now
storeIfCreateDep :: MetaDir -> Target -> IO ()
storeIfCreateDep metaDepsDir dep = bool (createEmptyMetaFile $ ifCreateMetaFile metaDepsDir dep) 
  (putErrorStrLn ("Error: Running redo-ifcreate on '" ++ unTarget dep ++ "' failed because it already exists.") >> exitFailure) =<< doesTargetExist dep

storeAlwaysDep :: MetaDir -> IO ()
storeAlwaysDep metaDepsDir = createEmptyMetaFile $ alwaysMetaFile metaDepsDir

--storePhonyTarget :: MetaDir -> IO () 
--storePhonyTarget metaDepsDir = createEmptyMetaFile $ phonyFile metaDepsDir

storePhonyTarget :: Key -> IO () 
storePhonyTarget key = do
  phonyTargetDir <- getPhonyTargetDirectory key
  writeMetaFile' phonyTargetDir (escapeDependencyPath '@' $ ".")

markTargetClean :: Key -> IO ()
markTargetClean key =
  --removeSessionFiles metaDepsDir -- We don't need to do this, so I am optmizing it out since it take a long time
  createEmptyMetaFile =<< cleanFile key

markTargetDirty :: Key -> IO ()
markTargetDirty key =
  --removeSessionFiles metaDepsDir -- We don't need to do this, so I am optmizing it out since it take a long time
  createEmptyMetaFile =<< dirtyFile key

--markTargetBuilt :: Target -> MetaDir -> IO ()
--markTargetBuilt target metaDepsDir = do
--  timestamp <- getStamp target
--  writeMetaFile (builtFile metaDepsDir) timestamp

-- Cache the do file path so we know which do was used to build a target the last time it was built
-- TODO remove
--cacheDoFile :: MetaDir -> DoFile -> IO ()
--cacheDoFile metaDepsDir doFile = writeFile (unMetaFile $ doFileCache metaDepsDir) (unDoFile doFile)

cacheDoFile :: Key -> DoFile -> IO ()
cacheDoFile key doFile = do
  doFileDir <- getDoFileDirectory key
  writeMetaFile' doFileDir (escapeDependencyPath '@' $ unDoFile doFile)

storeStamp :: Key -> Target -> IO ()
storeStamp key target = do
  timestamp <- getStamp target
  stampDir <- getStampDirectory key
  -- TODO fix this the redundant unpack here
  writeMetaFile' stampDir (escapeDependencyPath '@' $ BS.unpack $ unStamp timestamp)

-- Return the lock file name for a target:
-- TODO make this take a key
createLockFile :: Target -> IO LockFile
createLockFile target = do
  key <- getKey target
  createLockFile' key

createLockFile' :: Key -> IO LockFile
createLockFile' key = do 
  lockFileDir <- getLockFileDirectory key
  -- TODO cleanup:
  safeCreateDirectoryRecursive lockFileDir 
  return $ LockFile $ lockFileDir </> "l"

---------------------------------------------------------------------
-- Functions reading meta files:
---------------------------------------------------------------------
readMetaFile :: MetaFile -> IO Stamp
readMetaFile file = Stamp <$> BS.readFile (unMetaFile file)

-- Get the cached timestamp for when a target was last built. Return '.'
getTargetBuiltTimeStamp :: MetaDir -> IO (Maybe Stamp)
getTargetBuiltTimeStamp metaDepsDir = catch (Just <$> readMetaFile (builtFile metaDepsDir)) 
  (\(_ :: SomeException) -> return Nothing)

isTargetMarkedClean :: Key -> IO Bool 
isTargetMarkedClean key = doesMetaFileExist =<< cleanFile key

isTargetMarkedDirty :: Key -> IO Bool 
isTargetMarkedDirty key = doesMetaFileExist =<< dirtyFile key

-- Retrieve the cached do file path inside meta dir
-- getCachedDoFile :: MetaDir -> IO (Maybe DoFile)
-- getCachedDoFile metaDepsDir = catch (readCache cache) (\(_ :: SomeException) -> return Nothing)
--   where readCache cachedDo = do doFile <- readFile cachedDo
--                                 return $ Just $ DoFile doFile
--         cache = unMetaFile $ doFileCache metaDepsDir

-- Retrieve the cached do file path inside meta dir
getCachedDoFile :: Key -> IO (Maybe DoFile)
getCachedDoFile key = do
  doFileDir <- getDoFileDirectory key
  catch(readDoFile doFileDir) (\(_ :: SomeException) -> return Nothing)
  where readDoFile dir = do doFile <- readMetaFile' dir
                            return $ Just $ (DoFile $ unEscapeDependencyPath '@' doFile)

-- Returns the path to the target, if it exists, otherwise it returns the path to the
-- phony target if it exists, else return Nothing
getBuiltTargetPath :: MetaDir -> Target -> IO(Maybe Target)
getBuiltTargetPath metaDepsDir target = do
  -- TODO cleanup, this function should take a key
  key <- getKey target
  phonyF <- phonyFile key
  returnTargetIfExists (returnPhonyIfExists (return Nothing) (phonyF)) target
  where returnTargetIfExists failFunc file = bool failFunc (return $ Just file) =<< doesTargetExist file
        returnPhonyIfExists failFunc file = bool failFunc (return $ Just $ Target $ unMetaFile file) =<< doesMetaFileExist file

getBuiltTargetPath' :: Target -> IO(Maybe Target)
getBuiltTargetPath' target = do metaDepsDir <- metaDir target
                                getBuiltTargetPath metaDepsDir target

-- Does the target file or directory exist on the filesystem?
-- Checks if a target file is a buildable target, or if it is a source file
isSourceFile :: Target -> IO Bool
isSourceFile target = bool (return False) (not <$> hasDependencies target) =<< doesTargetExist target
  where
    -- Check's if a target has dependencies stored already
    hasDependencies :: Target -> IO Bool
    hasDependencies t = doesMetaDirExist =<< metaDir t

---------------------------------------------------------------------
-- Functions deleting meta files:
---------------------------------------------------------------------
-- Check for stored clean or dirty files in a meta dir.
-- Remove dirty and clean files in a meta dir.
-- Optimization: running this function takes a long time. We don't have to run it, so I am no longer using it.
-- Not running this means that session files will begin to build up in the meta directories. However, once
-- a target is rebuilt, all old session files are removed anyways. If enough session files build up (think 1000s)
-- then maybe running upToDate will be slower... but that is probably not the common case, so removing this function
-- for now.
-- removeSessionFiles :: MetaDir -> IO ()
-- removeSessionFiles metaDepsDir = safeRemoveGlob metaDepsDir' "*.cln." >> safeRemoveGlob metaDepsDir' "*.drt." 
--   where metaDepsDir' = unMetaDir metaDepsDir

-- TODO remove
--removeLockFiles :: IO ()
--removeLockFiles = do dir <- redoMetaDirectory 
--                     safeRemoveGlob dir "*.lck."

---------------------------------------------------------------------
-- Functions creating meta file names
---------------------------------------------------------------------
-- Form the hash file path for a target's dependency given the current target meta dir and the target's dependency
getMetaFile :: (FilePath -> FilePath) -> MetaDir -> Target -> MetaFile
getMetaFile escapeFunc metaDepsDir dependency = MetaFile $ unMetaDir metaDepsDir </> escapeFunc (unTarget dependency)

-- Functions to get the dependency path for each file type
ifChangeMetaFile :: MetaDir -> Target -> MetaFile 
ifChangeMetaFile = getMetaFile escapeIfChangePath
ifCreateMetaFile :: MetaDir -> Target -> MetaFile 
ifCreateMetaFile = getMetaFile escapeIfCreatePath
alwaysMetaFile :: MetaDir -> MetaFile 
alwaysMetaFile depDir = MetaFile $ unMetaDir depDir </> file
  where file = "." ++ [always_dependency_prepend] ++ "redo-always" ++ [always_dependency_prepend] ++ "."

--phonyFile :: MetaDir -> MetaFile
--phonyFile metaDepsDir = MetaFile $ unMetaDir metaDepsDir </> "." ++ "phony-target" ++ "."

-- TODO remove
phonyFile :: Key -> IO MetaFile
phonyFile key = do
  phonyTargetDir <- getPhonyTargetDirectory key
  return $ MetaFile $ phonyTargetDir

doFileCache :: MetaDir -> MetaFile  
doFileCache metaDepsDir = MetaFile $ unMetaDir metaDepsDir </> ".do.do."

--cleanFile :: MetaDir -> IO MetaFile  
--cleanFile metaDepsDir = f metaDepsDir =<< getEnv "REDO_SESSION"
--  where f depDir session = return $ MetaFile $ unMetaDir depDir </> ".cln." ++ session  ++ ".cln."

cleanFile :: Key -> IO MetaFile  
cleanFile key = do
  cacheDir <- getCacheDirectory key
  return $ MetaFile $ cacheDir </> "c"

dirtyFile :: Key -> IO MetaFile  
dirtyFile key = do
  cacheDir <- getCacheDirectory key
  return $ MetaFile $ cacheDir </> "d"

--dirtyFile :: MetaDir -> IO MetaFile 
--dirtyFile metaDepsDir = f metaDepsDir =<< getEnv "REDO_SESSION"
--  where f depDir session = return $ MetaFile $ unMetaDir depDir </> ".drt." ++ session  ++ ".drt."

-- Construct file for storing built timestamp
builtFile :: MetaDir -> MetaFile  
builtFile metaDepsDir = MetaFile $ unMetaDir metaDepsDir </> ".blt.blt."

---------------------------------------------------------------------
-- Functions escaping and unescaping path names
---------------------------------------------------------------------
ifChangeMetaFileToTarget :: FilePath -> MetaFile -> Target
ifChangeMetaFileToTarget doDirectory metaFile = Target $ removeDotDirs $ doDirectory </> unEscapeIfChangePath (unMetaFile metaFile)

ifCreateMetaFileToTarget :: FilePath -> MetaFile -> Target
ifCreateMetaFileToTarget doDirectory metaFile = Target $ removeDotDirs $ doDirectory </> unEscapeIfCreatePath (unMetaFile metaFile)

-- This is the same as running normalise, but it always removes the trailing path
-- separator, and it always keeps a "./" in front of things in the current directory
-- and always removes "./" in front of things not in the current directory.
-- we use this to ensure consistancy of naming convention
sanitizeFilePath :: FilePath -> FilePath
sanitizeFilePath filePath = normalise $ dir </> file
  where (dir, file) = splitFileName . dropTrailingPathSeparator . normalise $ filePath

-- Takes a file path and replaces all </> with @
escapeDependencyPath :: Char -> FilePath -> FilePath
escapeDependencyPath dependency_prepend path = (['.'] ++ [dependency_prepend]) ++ concatMap repl path' ++ ([dependency_prepend] ++ ['.'])
  where path' = sanitizeFilePath path
        repl seperator_replacement = seperator_replacement : [seperator_replacement_escape]
        repl c   = if isPathSeparator c then [seperator_replacement] else [c]

-- Reverses escapeFilePath
unEscapeDependencyPath :: Char -> FilePath -> FilePath
unEscapeDependencyPath dependency_prepend name = sanitizeFilePath path
  where 
    path = if take 2 name == ('.' : [dependency_prepend]) then unEscape $ (dropEnd 2 . drop 2) name else name
    dropEnd n list = take (length list - n) list
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

-- Functions to escape and unescape dependencies of different types:
escapeIfChangePath :: FilePath -> FilePath
escapeIfChangePath = escapeDependencyPath ifchange_dependency_prepend
unEscapeIfChangePath :: FilePath -> FilePath 
unEscapeIfChangePath = unEscapeDependencyPath ifchange_dependency_prepend
escapeIfCreatePath :: FilePath -> FilePath
escapeIfCreatePath = escapeDependencyPath ifcreate_dependency_prepend
unEscapeIfCreatePath :: FilePath -> FilePath 
unEscapeIfCreatePath = unEscapeDependencyPath ifcreate_dependency_prepend

---------------------------------------------------------------------
-- Higher level functions
---------------------------------------------------------------------
-- Store dependencies for redo-ifchange:
storeIfChangeDependencies :: [Target] -> IO ()
storeIfChangeDependencies = storeDependencies storeIfChangeDep

-- Store dependencies for redo-ifcreate:
storeIfCreateDependencies :: [Target] -> IO ()
storeIfCreateDependencies = storeDependencies storeIfCreateDep

-- Return some redo environment vaiables
getRedoEnv :: IO (FilePath, MetaDir)
getRedoEnv = do
  parentRedoPath <- getEnv "REDO_PATH" -- directory where .do file was run from
  parentRedoTarget <- getEnv "REDO_TARGET"
  parentRedoMetaDir <- metaDir $ Target parentRedoTarget
  return (parentRedoPath, parentRedoMetaDir)

-- Store dependency for redo-always:
storeAlwaysDependency :: IO ()
storeAlwaysDependency = do 
  (_, parentRedoMetaDir) <- getRedoEnv
  storeAlwaysDep parentRedoMetaDir

-- Store dependencies given a store action and a list of dependencies to store:
storeDependencies :: (MetaDir -> Target -> IO ()) -> [Target] -> IO ()  
storeDependencies storeAction dependencies = do 
  (parentRedoPath, parentRedoMetaDir) <- getRedoEnv
  dependenciesRel2Parent <- makeRelativeToParent parentRedoPath dependencies 
  mapM_ (storeAction parentRedoMetaDir) dependenciesRel2Parent
  where
    makeRelativeToParent :: FilePath -> [Target] -> IO [Target]
    makeRelativeToParent parent targets = do
      currentDir <- getCurrentDirectory
      -- Note: All target listed here are relative to the current directory in the .do script. This could
      -- be different than the REDO_PATH variable, which represents the directory where the .do was invoked 
      -- if 'cd' was used in the .do script.
      -- So, let's get a list of targets relative to the parent .do file invocation location, REDO_PATH
      return $ map (Target . makeRelative parent . (currentDir </>) . unTarget) targets
