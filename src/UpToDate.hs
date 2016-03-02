{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UpToDate (upToDate) where

import Data.Maybe (isNothing, fromJust)
import System.FilePath (takeDirectory, takeExtension, (</>))

import Helpers
import Types
import Database 
import PrettyPrint

---------------------------------------------------------------------
-- Functions checking if a target or its dependencies are up to date
---------------------------------------------------------------------
-- Top upToDate which should be called by redo-ifchange. Return true if a file is clean and does
-- not need to be built. Return false if a file is dirty and needs to be rebuilt.
-- Note: target must be the absolute canonicalized path to the target
upToDate :: Target -> IO Bool
upToDate = upToDate' 0

upToDate' :: Int -> Target -> IO Bool
upToDate' level target = do
  return () `debug'` "=checking"
  key <- getKey target
  databaseExists <- doesDatabaseExist key
  -- If there is no database for this target and it doesn't exist than it has never been built, or it is
  -- a source file that we have never seen before. Either way, return False.
  if not databaseExists then return False `debug'` "-new target"
  else do
    existingTarget <- getBuiltTargetPath key target
    -- If neither a target or a phony target exists, then the target is obviously not up to date
    if isNothing existingTarget then return False `debug'` "-not built"
    else do
      clean <- isClean key  
      -- If we have already checked off this target as up to date, there is no need to check again
      if clean then return True `debug'` "+clean"
      else do
        dirty <- isDirty key 
        -- If we have already checked off this target as dirty, don't delay, return not up to date
        if dirty then return False `debug'` "-dirty"
        else do 
          cachedStamp <- getStamp key
          currentStamp <- safeStampTarget (fromJust existingTarget)
          --putWarningStrLn $ "target: " ++ unTarget target
          --putWarningStrLn $ "cachedStamp : " ++ show cachedStamp
          --putWarningStrLn $ "currentStamp: " ++ show currentStamp
          -- The target has been modified because the timestamps dont match
          if cachedStamp /= currentStamp then returnFalse key `debug'` "-modified"
          else do 
            ret <- upToDate'' level target key
            if ret then returnTrue key else returnFalse key
  where
    -- Convenient debug function:
    debug' = debugUpToDate level target
    -- Helper function which returns true and marks the target as clean:
    returnTrue :: Key -> IO Bool
    returnTrue key = markClean key >> return True
    -- Helper function which returns false and marks the target as dirty:
    returnFalse :: Key -> IO Bool
    returnFalse key = markDirty key >> return False
    
upToDate'' :: Int -> Target -> Key -> IO Bool
upToDate'' level target key = do
  source <- isSource key  
  if source then return True `debug'` "+source"
  else do
    doFile <- findDoFile target
    -- If no do file is found, but the database exists, than this file used to be buildable, but is
    -- now a newly marked source file.
    if isNothing doFile then return False `debug'` "+removed do"
    else do
      let absDoFile = fromJust doFile
      newDo <- newDoFile absDoFile
      -- If the target exists but a new do file was found for it then we need to rebuilt it, so
      -- it is not up to date.
      if newDo then return False `debug'` "-new do"
      else do
        let doFileDir = takeDirectory $ unDoFile absDoFile
        -- If all of the dependencies are up to date then this target is also up to date, so mark it
        -- as such and return true. Else, return false.
        depsUpToDate (level+1) target doFileDir key
  where 
    debug' = debugUpToDate level target
    -- Does the target have a new do file from the last time it was built?
    newDoFile :: DoFile -> IO Bool
    newDoFile doFile = do
      -- We shouldn't expect a do file to build another do file by default, so skip this check
      -- otherwise we end up with uncorrect behavior
      if takeExtension (unTarget target) == ".do" then return False
      else maybe (return True) (pathsNotEqual doFile) =<< getDoFile key
      where pathsNotEqual path1 path2 = if path1 /= path2 then return True else return False

-- Are a target's redo-create or redo-always or redo-ifchange dependencies up to date? 
-- If so return, true, otherwise return false. Note that this function recurses on a target's
-- dependencies to make sure the dependencies are up to date.
depsUpToDate :: Int -> Target -> FilePath -> Key -> IO Bool
depsUpToDate level target doFileDir key = do
  -- redo-always - if an always dependency exists, we need to return False immediately
  alwaysDeps <- hasAlwaysDep key
  if alwaysDeps then return False `debug'` "-dep always"
  else do 
    -- redo-ifcreate - if one of those files was created, we need to return False immediately
    ifCreateDeps <- getIfCreateDeps key
    depCreated' <- mapOr (doesTargetExist . makeAbsolute) ifCreateDeps
    if depCreated' then return False `debug'` "-dep created"
    else do
      -- redo-ifchange - check these files hashes against those stored to determine if they are up to date
      --                 then recursively check their dependencies to see if they are up to date
      -- TODO use this function soon
      ifChangeDeps <- getIfChangeDeps key
      --ifChangeDeps <- getIfChangeDeps'' key
      mapAnd ((upToDate' (level+1)) . makeAbsolute) ifChangeDeps 
  where 
    makeAbsolute metaFile = Target $ removeDotDirs $ doFileDir </> (unMetaFile metaFile)
    debug' = debugUpToDate level target

-- Are a target's redo-ifchange dependencies up to date?
--ifChangeDepsUpToDate :: Int -> Key -> FilePath -> MetaFile -> IO Bool
--ifChangeDepsUpToDate level parentKey doDir hashFile = do
--  return () `debug'` "=checking"
--  depKey <- getKey dep
--  databaseExists <- doesDatabaseExist depKey
--  targetExists <- doesTargetExist dep
--  parentIfChangeEntry <- getIfChangeEntry parentKey
--  let  hashFullPath = Entry $ entryToFilePath parentIfChangeEntry </> unMetaFile hashFile
--  case (targetExists, databaseExists) of 
--    -- If no meta data for this target is stored and it doesn't exist than it has never been built
--    (False, False) -> return False `debug'` "-never built"
--    -- If the target exists on the filesystem but does not have meta deps dir then redo never
--    -- created it. It must be a source file so we need to check its stamp
--    (True, False) -> do hashesMatch <- compareStamp hashFullPath dep 
--                        if hashesMatch then return True `debug'` "+unchanged"
--                        else return False `debug'` "-changed"                              
--    -- If the meta deps dir exists, then we need to check extra info contained within it to determine
--    -- if the target is up to date:
--    (_, True) -> do
--      existingTarget <- getBuiltTargetPath depKey dep
--      -- If neither a target or a phony target exists, then the target is obviously not up to date
--      if isNothing existingTarget then returnFalse depKey `debug'` "-not built"
--      else do
--        clean <- isClean depKey
--        -- If we have already checked off this target as up to date, there is no need to check again
--        if clean then return True `debug'` "+clean"
--        else do 
--          dirty <- isDirty depKey 
--          -- If we have already checked off this target as dirty, don't delay, return not up to date
--          if dirty then return False `debug'` "-dirty"
--          else do
--            cachedStamp <- getStamp depKey
--            currentStamp <- safeStampTarget dep
--            whenEqualOrNothing cachedStamp currentStamp (return False `debug'` "-modified") (do
--              -- Check the target against it's stored hash
--              hashesMatch <- compareStamp hashFullPath (fromJust existingTarget)
--              if hashesMatch then upToDate' level dep depKey
--              else return False `debug'` "-dep changed")
--  where
--    debug' = debugUpToDate level dep
--    --hashFullPath = Entry $ unMetaDir parentMetaDir </> "r" </> unMetaFile hashFile
--    dep = ifChangeMetaFileToTarget doDir hashFile
--    -- Check the hash of the dependency and compare it to the stored hash. This function provides recursion:
--    compareStamp :: Entry -> Target -> IO Bool
--    compareStamp storedStamp fileToStamp = do
--      --oldStamp <- readMetaFile storedStamp
--      -- TODO fix this
--      oldStamp <- safeGetOldStamp
--      newStamp <- stampTarget fileToStamp
--      return $ (Stamp oldStamp) == newStamp
--        -- TODO i hope i can remove this      
--        where safeGetOldStamp = catch (do contents <- readEntry1 storedStamp
--                                          return $ unEscapeDependencyPath '@' contents) (\(_ :: SomeException) -> return "")

-- Helper for debugging:
debugUpToDate :: Int -> Target -> c -> String -> c
debugUpToDate depth file a string = debug a (createSpaces (depth*2) ++ string ++ createSpaces paddingToAppend ++ " -- " ++ unTarget file)
  where createSpaces num = concat $ replicate num " "
        stringWidth = 12 
        paddingToAppend = stringWidth - length string
                
