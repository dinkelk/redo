{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module SqliteDb
  ( withRedoDb
  , dbInitTarget
  , dbInitSource
  , dbExists
  , dbGetStamp
  , dbStoreStamp
  , dbGetDoFile
  , dbGetIfChangeDeps
  , dbStoreIfChangeDep
  , dbGetIfCreateDeps
  , dbStoreIfCreateDep
  , dbHasAlwaysDep
  , dbStoreAlwaysDep
  , dbIsSource
  , dbIsErrored
  , dbMarkErrored
  , dbStorePhony
  , dbGetPhony
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch, throwIO, SomeException(..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite3
import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

---------------------------------------------------------------------
-- Cached connection (one per process, opened lazily)
---------------------------------------------------------------------
{-# NOINLINE cachedConn #-}
cachedConn :: IORef (Maybe Database)
cachedConn = unsafePerformIO $ newIORef Nothing

withRedoDb :: (Database -> IO a) -> IO a
withRedoDb action = do
  mc <- readIORef cachedConn
  case mc of
    Just db -> action db
    Nothing -> do
      dir <- getAppUserDataDirectory "redo"
      createDirectoryIfMissing True dir
      db <- open (T.pack $ dir </> "redo.sqlite3")
      -- WAL mode allows concurrent readers + one writer
      exec db "PRAGMA journal_mode=WAL"
      exec db "PRAGMA synchronous=NORMAL"
      exec db "PRAGMA busy_timeout=5000"
      initSchema db
      writeIORef cachedConn (Just db)
      action db

initSchema :: Database -> IO ()
initSchema db = do
  exec db $ T.concat
    [ "CREATE TABLE IF NOT EXISTS targets ("
    , "key TEXT PRIMARY KEY,"
    , "is_source INTEGER DEFAULT 0,"
    , "is_errored INTEGER DEFAULT 0,"
    , "stamp TEXT,"
    , "do_file TEXT,"
    , "phony TEXT,"
    , "always_dep INTEGER DEFAULT 0)"
    ]
  exec db $ T.concat
    [ "CREATE TABLE IF NOT EXISTS ifchange_deps ("
    , "key TEXT NOT NULL,"
    , "dep TEXT NOT NULL)"
    ]
  exec db "CREATE INDEX IF NOT EXISTS idx_ifc ON ifchange_deps(key)"
  exec db $ T.concat
    [ "CREATE TABLE IF NOT EXISTS ifcreate_deps ("
    , "key TEXT NOT NULL,"
    , "dep TEXT NOT NULL)"
    ]
  exec db "CREATE INDEX IF NOT EXISTS idx_ifcr ON ifcreate_deps(key)"

---------------------------------------------------------------------
-- Retry logic for concurrent writes
---------------------------------------------------------------------
retryOnBusy :: IO a -> IO a
retryOnBusy action = go (50 :: Int)
  where
    go 0 = action
    go n = catch action $ \(e :: SomeException) ->
      if "ErrorBusy" `T.isInfixOf` T.pack (show e)
        then threadDelay 10000 >> go (n - 1)
        else throwIO e

---------------------------------------------------------------------
-- Helper: run a query with params, return rows
---------------------------------------------------------------------
queryOne :: Database -> Text -> [SQLData] -> IO [SQLData]
queryOne db sql params = do
  stmt <- prepare db sql
  bind stmt params
  r <- step stmt
  row <- case r of
    Row  -> columns stmt
    Done -> return []
  finalize stmt
  return row

queryAll :: Database -> Text -> [SQLData] -> IO [[SQLData]]
queryAll db sql params = do
  stmt <- prepare db sql
  bind stmt params
  rows <- collectRows stmt
  finalize stmt
  return rows
  where
    collectRows s = do
      r <- step s
      case r of
        Row  -> do row <- columns s
                   rest <- collectRows s
                   return (row : rest)
        Done -> return []

execBind :: Database -> Text -> [SQLData] -> IO ()
execBind db sql params = retryOnBusy $ do
  stmt <- prepare db sql
  bind stmt params
  _ <- step stmt
  finalize stmt

---------------------------------------------------------------------
-- Operations
---------------------------------------------------------------------

dbInitTarget :: Database -> Text -> Text -> IO ()
dbInitTarget db key doFile = retryOnBusy $ do
  exec db "BEGIN IMMEDIATE"
  -- NOTE: preserve stamp across rebuilds (matches old filesystem behavior where stamps were separate)
  s1 <- prepare db "INSERT INTO targets(key,is_source,is_errored,do_file,phony,always_dep) VALUES(?1,0,0,?2,NULL,0) ON CONFLICT(key) DO UPDATE SET is_source=0,is_errored=0,do_file=?2,phony=NULL,always_dep=0"
  bind s1 [SQLText key, SQLText doFile]
  _ <- step s1
  finalize s1
  s2 <- prepare db "DELETE FROM ifchange_deps WHERE key=?1"
  bind s2 [SQLText key]
  _ <- step s2
  finalize s2
  s3 <- prepare db "DELETE FROM ifcreate_deps WHERE key=?1"
  bind s3 [SQLText key]
  _ <- step s3
  finalize s3
  s4 <- prepare db "INSERT INTO ifchange_deps(key,dep) VALUES(?1,?2)"
  bind s4 [SQLText key, SQLText doFile]
  _ <- step s4
  finalize s4
  exec db "COMMIT"

dbInitSource :: Database -> Text -> Text -> IO ()
dbInitSource db key stampVal =
  execBind db "INSERT INTO targets(key,is_source,stamp) VALUES(?1,1,?2) ON CONFLICT(key) DO UPDATE SET is_source=1,stamp=?2"
    [SQLText key, SQLText stampVal]

dbExists :: Database -> Text -> IO Bool
dbExists db key = do
  row <- queryOne db "SELECT 1 FROM targets WHERE key=?1 LIMIT 1" [SQLText key]
  return $ not (null row)

dbGetStamp :: Database -> Text -> IO (Maybe Text)
dbGetStamp db key = do
  row <- queryOne db "SELECT stamp FROM targets WHERE key=?1 LIMIT 1" [SQLText key]
  case row of
    [SQLText s] -> return (Just s)
    _           -> return Nothing

dbStoreStamp :: Database -> Text -> Text -> IO ()
dbStoreStamp db key stampVal =
  execBind db "INSERT INTO targets(key,stamp) VALUES(?1,?2) ON CONFLICT(key) DO UPDATE SET stamp=?2"
    [SQLText key, SQLText stampVal]

dbGetDoFile :: Database -> Text -> IO (Maybe Text)
dbGetDoFile db key = do
  row <- queryOne db "SELECT do_file FROM targets WHERE key=?1 LIMIT 1" [SQLText key]
  case row of
    [SQLText s] -> return (Just s)
    _           -> return Nothing

dbGetIfChangeDeps :: Database -> Text -> IO [Text]
dbGetIfChangeDeps db key = do
  rows <- queryAll db "SELECT dep FROM ifchange_deps WHERE key=?1" [SQLText key]
  return [t | [SQLText t] <- rows]

dbStoreIfChangeDep :: Database -> Text -> Text -> IO ()
dbStoreIfChangeDep db key dep =
  execBind db "INSERT INTO ifchange_deps(key,dep) VALUES(?1,?2)" [SQLText key, SQLText dep]

dbGetIfCreateDeps :: Database -> Text -> IO [Text]
dbGetIfCreateDeps db key = do
  rows <- queryAll db "SELECT dep FROM ifcreate_deps WHERE key=?1" [SQLText key]
  return [t | [SQLText t] <- rows]

dbStoreIfCreateDep :: Database -> Text -> Text -> IO ()
dbStoreIfCreateDep db key dep =
  execBind db "INSERT INTO ifcreate_deps(key,dep) VALUES(?1,?2)" [SQLText key, SQLText dep]

dbHasAlwaysDep :: Database -> Text -> IO Bool
dbHasAlwaysDep db key = do
  row <- queryOne db "SELECT always_dep FROM targets WHERE key=?1 LIMIT 1" [SQLText key]
  case row of
    [SQLInteger 1] -> return True
    _              -> return False

dbStoreAlwaysDep :: Database -> Text -> IO ()
dbStoreAlwaysDep db key =
  execBind db "INSERT INTO targets(key,always_dep) VALUES(?1,1) ON CONFLICT(key) DO UPDATE SET always_dep=1" [SQLText key]

dbIsSource :: Database -> Text -> IO Bool
dbIsSource db key = do
  row <- queryOne db "SELECT is_source FROM targets WHERE key=?1 LIMIT 1" [SQLText key]
  case row of
    [SQLInteger 1] -> return True
    _              -> return False

dbIsErrored :: Database -> Text -> IO Bool
dbIsErrored db key = do
  row <- queryOne db "SELECT is_errored FROM targets WHERE key=?1 LIMIT 1" [SQLText key]
  case row of
    [SQLInteger 1] -> return True
    _              -> return False

dbMarkErrored :: Database -> Text -> IO ()
dbMarkErrored db key =
  execBind db "INSERT INTO targets(key,is_errored) VALUES(?1,1) ON CONFLICT(key) DO UPDATE SET is_errored=1" [SQLText key]

dbStorePhony :: Database -> Text -> Text -> IO ()
dbStorePhony db key phony =
  execBind db "INSERT INTO targets(key,phony) VALUES(?1,?2) ON CONFLICT(key) DO UPDATE SET phony=?2" [SQLText key, SQLText phony]

dbGetPhony :: Database -> Text -> IO (Maybe Text)
dbGetPhony db key = do
  row <- queryOne db "SELECT phony FROM targets WHERE key=?1 LIMIT 1" [SQLText key]
  case row of
    [SQLText s] -> return (Just s)
    _           -> return Nothing
