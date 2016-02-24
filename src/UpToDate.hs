{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UpToDate (upToDate) where

import Data.Maybe (isNothing, fromJust)
import System.FilePath ((</>), takeDirectory, takeExtension)

import Helpers
import Types
import MetaDirectory

---------------------------------------------------------------------
-- Functions checking if a target or its dependencies are up to date
---------------------------------------------------------------------
-- Top upToDate which should be called by redo-ifchange. Return true if a file is clean and does
-- not need to be built. Return false if a file is dirty and needs to be rebuilt.
-- Note: target must be the absolute canonicalized path to the target
upToDate :: Target -> IO Bool
upToDate target = do
  depDir <- metaDir target
  return () `debug'` "=checking"
  hasMetaDeps <- doesMetaDirExist depDir
  targetExists <- doesTargetExist target
  case (targetExists, hasMetaDeps) of 
    -- If no meta data for this target is stored and it doesn't exist than it has never been built
    (False, False) -> return False `debug'` "-never built"
    -- If the target exists on the filesystem but does not have meta deps dir then redo never
    -- created it. It must be a source file so it is up to date.
    (True, False) -> return True `debug'` "+source"
    -- If the meta deps dir exists, then we need to check extra info contained within it to determine
    -- if the target is up to date:
    (_, True) -> do
      existingTarget <- getBuiltTargetPath depDir target
      -- If neither a target or a phony target exists, then the target is obviously not up to date
      if isNothing existingTarget then returnFalse depDir `debug'` "-not built"
      else do
        dirty <- isTargetMarkedDirty depDir
        -- If we have already checked off this target as dirty, don't delay, return not up to date
        if dirty then return False `debug'` "-dirty"
        else do
          clean <- isTargetMarkedClean depDir
          -- If we have already checked off this target as up to date, there is no need to check again
          if clean then return True `debug'` "+clean"
          else do 
            cachedTimeStamp <- getTargetBuiltTimeStamp depDir
            currentTimeStamp <- safeGetStamp target
            whenEqualOrNothing cachedTimeStamp currentTimeStamp 
              -- The target has been modified because the timestamps dont match
              (return False `debug'` "-modified") 
              -- the target hasn't been modified because the timestamps do match, or one of the timestamps is nothing
              (upToDate' 0 target depDir)
  where
    -- Convenient debug function:
    debug' = debugUpToDate 0 target
    
upToDate' :: Int -> Target -> MetaDir -> IO Bool
upToDate' level target depDir = do
  doFile <- findDoFile target
  -- If no do file is found, but the meta dir exists, than this file used to be buildable, but is
  -- now a newly marked source file. So remove the meta dir but return false to be conservative. 
  -- There is no need to mark the file clean because the meta dir is removed.
  if isNothing doFile then (removeMetaDir depDir >> return False) `debug'` "+new source"
  else do
    let absDoFile = fromJust doFile
    newDo <- newDoFile depDir absDoFile
    -- If the target exists but a new do file was found for it then we need to rebuilt it, so
    -- it is not up to date.
    if newDo then returnFalse depDir `debug'` "-new .do"
    else do
      let doFileDir = takeDirectory $ unDoFile absDoFile
      -- If all of the dependencies are up to date then this target is also up to date, so mark it
      -- as such and return true. Else, return false.
      depsClean <- depsUpToDate (level+1) target depDir doFileDir 
      if depsClean then returnTrue depDir -- `debug'` "+deps clean"
      else returnFalse depDir -- `debug'` "-deps dirty "
  where 
    debug' = debugUpToDate level target
    -- Does the target have a new do file from the last time it was built?
    newDoFile :: MetaDir -> DoFile -> IO Bool
    newDoFile metaDepsDir doFile =
      -- We shouldn't expect a do file to build another do file by default, so skip this check
      -- otherwise we end up with uncorrect behavior
      if takeExtension (unTarget target) == ".do" then return False
      else maybe (return True) (pathsNotEqual doFile) =<< getCachedDoFile metaDepsDir
      where pathsNotEqual path1 path2 = if path1 /= path2 then return True else return False

-- Are a target's redo-create or redo-always or redo-ifchange dependencies up to date? 
-- If so return, true, otherwise return false. Note that this function recurses on a target's
-- dependencies to make sure the dependencies are up to date.
depsUpToDate :: Int -> Target -> MetaDir -> FilePath ->  IO Bool
depsUpToDate level target metaDepsDir doFileDir = do
  (ifChangeDeps, ifCreateDeps, ifAlwaysDeps) <- getMetaDirDependencies metaDepsDir
  if not $ null ifAlwaysDeps then return False `debug'` "-dep always"
  else do 
    -- redo-ifcreate - if one of those files was created, we need to return False immediately
    depCreated' <- mapOr (doesTargetExist . ifCreateMetaFileToTarget doFileDir) ifCreateDeps
    if depCreated' then return False `debug'` "-dep created"
    -- redo-ifchange - check these files hashes against those stored to determine if they are up to date
    --                 then recursively check their dependencies to see if they are up to date
    else mapAnd (ifChangeDepsUpToDate level metaDepsDir doFileDir) ifChangeDeps 
  where 
    debug' = debugUpToDate level target

-- Are a target's redo-ifchange dependencies up to date?
ifChangeDepsUpToDate :: Int -> MetaDir -> FilePath -> MetaFile -> IO Bool
ifChangeDepsUpToDate level parentMetaDir doDir hashFile = do
  depDir <- metaDir dep
  return () `debug'` "=checking"
  hasMetaDeps <- doesMetaDirExist depDir
  targetExists <- doesTargetExist dep
  case (targetExists, hasMetaDeps) of 
    -- If no meta data for this target is stored and it doesn't exist than it has never been built
    (False, False) -> return False `debug'` "-never built"
    -- If the target exists on the filesystem but does not have meta deps dir then redo never
    -- created it. It must be a source file so we need to check its stamp
    (True, False) -> do hashesMatch <- compareStamp hashFullPath dep 
                        if hashesMatch then return True `debug'` "+unchanged"
                        else return False `debug'` "-changed"                              
    -- If the meta deps dir exists, then we need to check extra info contained within it to determine
    -- if the target is up to date:
    (_, True) -> do
      existingTarget <- getBuiltTargetPath depDir dep
      -- If neither a target or a phony target exists, then the target is obviously not up to date
      if isNothing existingTarget then returnFalse depDir `debug'` "-not built"
      else do
        dirty <- isTargetMarkedDirty depDir
        -- If we have already checked off this target as dirty, don't delay, return not up to date
        if dirty then return False `debug'` "-dirty"
        else do
          clean <- isTargetMarkedClean depDir
          -- If we have already checked off this target as up to date, there is no need to check again
          if clean then return True `debug'` "+clean"
          else do 
            cachedTimeStamp <- getTargetBuiltTimeStamp depDir
            currentTimeStamp <- safeGetStamp dep
            whenEqualOrNothing cachedTimeStamp currentTimeStamp (return False `debug'` "-modified") (do
              -- Check the target against it's stored hash
              hashesMatch <- compareStamp hashFullPath (fromJust existingTarget)
              if hashesMatch then upToDate' level dep depDir
              else return False `debug'` "-dep changed")
  where
    debug' = debugUpToDate level dep
    hashFullPath = MetaFile $ unMetaDir parentMetaDir </> unMetaFile hashFile
    dep = ifChangeMetaFileToTarget doDir hashFile
    -- Check the hash of the dependency and compare it to the stored hash. This function provides recursion:
    compareStamp :: MetaFile -> Target -> IO Bool
    compareStamp storedStamp fileToStamp = do
      oldStamp <- readMetaFile storedStamp
      newStamp <- getStamp fileToStamp
      return $ oldStamp == newStamp

-- Helper function which returns true and marks the target as clean:
returnTrue :: MetaDir -> IO Bool
returnTrue metaDepsDir = markTargetClean metaDepsDir >> return True
-- Helper function which returns false and marks the target as dirty:
returnFalse :: MetaDir -> IO Bool
returnFalse metaDepsDir = markTargetDirty metaDepsDir >> return False

-- Helper for debugging:
debugUpToDate :: Int -> Target -> c -> String -> c
debugUpToDate depth file a string = debug a (createSpaces (depth*2) ++ string ++ createSpaces paddingToAppend ++ " -- " ++ unTarget file)
  where createSpaces num = concat $ replicate num " "
        stringWidth = 12
        paddingToAppend = stringWidth - length string
                
