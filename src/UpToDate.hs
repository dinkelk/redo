{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UpToDate (upToDate) where

import Data.Maybe (isNothing, fromJust)
import System.FilePath (takeExtension)
--import Debug.Trace (trace)

import Types
import Database 

-- Debug helpers:
debug :: c -> String -> c
--debug = flip trace
debug a _ = a

---------------------------------------------------------------------
-- Functions checking if a target or its dependencies are up to date
---------------------------------------------------------------------
-- Top upToDate which should be called by redo-ifchange. Return true if a file is clean and does
-- not need to be built. Return false if a file is dirty and needs to be rebuilt.
-- Note: target must be the absolute canonicalized path to the target
upToDate :: Key -> Target -> IO Bool
upToDate = upToDate'' 0

upToDate' :: Int -> Target -> IO Bool
upToDate' level target = do
  key <- getKey target
  upToDate'' level key target

upToDate'' :: Int -> Key -> Target -> IO Bool
upToDate'' level key target = do
  return () `debug'` "=checking"
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
          -- The target has been modified because the timestamps dont match
          if cachedStamp /= currentStamp then returnFalse `debug'` "-modified"
          else do 
            ret <- upToDate''' level target key
            if ret then returnTrue else returnFalse
  where
    -- Convenient debug function:
    debug' = debugUpToDate level target
    -- Helper function which returns true and marks the target as clean:
    returnTrue :: IO Bool
    returnTrue = markClean key >> return True
    -- Helper function which returns false and marks the target as dirty:
    returnFalse :: IO Bool
    returnFalse = markDirty key >> return False
    
upToDate''' :: Int -> Target -> Key -> IO Bool
upToDate''' level target key = do
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
      else depsUpToDate (level+1) target key
  where 
    debug' = debugUpToDate level target
    -- Does the target have a new do file from the last time it was built?
    newDoFile :: DoFile -> IO Bool
    newDoFile doFile =
      -- We shouldn't expect a do file to build another do file by default, so skip this check
      -- otherwise we end up with uncorrect behavior
      if takeExtension (unTarget target) == ".do" then return False
      else maybe (return True) (pathsNotEqual doFile) =<< getDoFile key
      where pathsNotEqual path1 path2 = if path1 /= path2 then return True else return False

-- Are a target's redo-create or redo-always or redo-ifchange dependencies up to date? 
-- If so return, true, otherwise return false. Note that this function recurses on a target's
-- dependencies to make sure the dependencies are up to date.
depsUpToDate :: Int -> Target -> Key -> IO Bool
depsUpToDate level target key = do
  -- redo-always - if an always dependency exists, we need to return False immediately
  alwaysDeps <- hasAlwaysDep key
  if alwaysDeps then return False `debug'` "-dep always"
  else do 
    -- redo-ifcreate - if one of those files was created, we need to return False immediately
    ifCreateDeps <- getIfCreateDeps key
    depCreated' <- mapOr (doesTargetExist) ifCreateDeps
    if depCreated' then return False `debug'` "-dep created"
    else do
      -- redo-ifchange - check these files hashes against those stored to determine if they are up to date
      --                 then recursively check their dependencies to see if they are up to date
      ifChangeDeps <- getIfChangeDeps key
      mapAnd (upToDate' (level+1)) ifChangeDeps 
  where 
    debug' = debugUpToDate level target

-- Helper for debugging:
debugUpToDate :: Int -> Target -> c -> String -> c
debugUpToDate depth file a string = debug a (createSpaces (depth*2) ++ string ++ createSpaces paddingToAppend ++ " -- " ++ unTarget file)
  where createSpaces num = concat $ replicate num " "
        stringWidth = 12 
        paddingToAppend = stringWidth - length string

-- Function which basically does "and `liftM` mapM" but has the optimization of not continuing evaluation
-- if a "False" is found. This helps prevent infinite loops if dependencies are circular.
mapAnd :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
mapAnd _ [] = return True
mapAnd func (x:xs) = do boolean <- func x
                        if boolean then mapAnd func xs
                        -- Optimization: cut the evaluation short if a single False is found
                        else return False
-- Function which basically does "or `liftM` mapM" but has the optimization of not continuing evaluation
-- if a "True" is found.
mapOr :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
mapOr _ [] = return False
mapOr func (x:xs) = do boolean <- func x
                       -- Optimization: cut the evaluation short if a single True is found
                       if boolean then return True
                       else mapOr func xs

