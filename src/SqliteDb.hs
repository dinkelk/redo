{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module SqliteDb
  ( withRedoDb
  , closeRedoDb
  , releaseRedoDbLocks
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
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, modifyMVar_)
import Control.Exception (bracketOnError, catch, throwIO)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite3
import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

---------------------------------------------------------------------
-- Database connection with cached prepared statements
---------------------------------------------------------------------
data DbConn = DbConn
  { dbHandle           :: !Database
  , stmtInitTarget     :: !Statement
  , stmtDeleteIfChange :: !Statement
  , stmtDeleteIfCreate :: !Statement
  , stmtInsertIfChange :: !Statement
  , stmtInitSource     :: !Statement
  , stmtExists         :: !Statement
  , stmtGetStamp       :: !Statement
  , stmtStoreStamp     :: !Statement
  , stmtGetDoFile      :: !Statement
  , stmtGetIfChangeDeps :: !Statement
  , stmtStoreIfChangeDep :: !Statement
  , stmtGetIfCreateDeps :: !Statement
  , stmtStoreIfCreateDep :: !Statement
  , stmtGetAlwaysDep   :: !Statement
  , stmtStoreAlwaysDep :: !Statement
  , stmtGetIsSource    :: !Statement
  , stmtGetIsErrored   :: !Statement
  , stmtMarkErrored    :: !Statement
  , stmtStorePhony     :: !Statement
  , stmtGetPhony       :: !Statement
  }

---------------------------------------------------------------------
-- Cached connection (one per process, opened lazily, thread-safe)
---------------------------------------------------------------------
{-# NOINLINE cachedConn #-}
cachedConn :: MVar (Maybe DbConn)
cachedConn = unsafePerformIO $ newMVar Nothing

openDbConn :: IO DbConn
openDbConn = do
  dir <- getAppUserDataDirectory "redo"
  createDirectoryIfMissing True dir
  db <- open (T.pack $ dir </> "redo.sqlite3")
  -- Set busy_timeout FIRST so subsequent statements retry on lock
  exec db "PRAGMA busy_timeout=5000"
  -- WAL mode allows concurrent readers + one writer
  retryOnBusy $ exec db "PRAGMA journal_mode=WAL"
  exec db "PRAGMA synchronous=NORMAL"
  retryOnBusy $ initSchema db
  -- Prepare all statements once
  DbConn db
    <$> prepare db "INSERT INTO targets(key,is_source,is_errored,do_file,phony,always_dep) VALUES(?1,0,0,?2,NULL,0) ON CONFLICT(key) DO UPDATE SET is_source=0,is_errored=0,do_file=?2,phony=NULL,always_dep=0"
    <*> prepare db "DELETE FROM ifchange_deps WHERE key=?1"
    <*> prepare db "DELETE FROM ifcreate_deps WHERE key=?1"
    <*> prepare db "INSERT INTO ifchange_deps(key,dep) VALUES(?1,?2)"
    <*> prepare db "INSERT INTO targets(key,is_source,stamp) VALUES(?1,1,?2) ON CONFLICT(key) DO UPDATE SET is_source=1,stamp=?2"
    <*> prepare db "SELECT 1 FROM targets WHERE key=?1 LIMIT 1"
    <*> prepare db "SELECT stamp FROM targets WHERE key=?1 LIMIT 1"
    <*> prepare db "INSERT INTO targets(key,stamp) VALUES(?1,?2) ON CONFLICT(key) DO UPDATE SET stamp=?2"
    <*> prepare db "SELECT do_file FROM targets WHERE key=?1 LIMIT 1"
    <*> prepare db "SELECT dep FROM ifchange_deps WHERE key=?1"
    <*> prepare db "INSERT OR IGNORE INTO ifchange_deps(key,dep) VALUES(?1,?2)"
    <*> prepare db "SELECT dep FROM ifcreate_deps WHERE key=?1"
    <*> prepare db "INSERT OR IGNORE INTO ifcreate_deps(key,dep) VALUES(?1,?2)"
    <*> prepare db "SELECT always_dep FROM targets WHERE key=?1 LIMIT 1"
    <*> prepare db "INSERT INTO targets(key,always_dep) VALUES(?1,1) ON CONFLICT(key) DO UPDATE SET always_dep=1"
    <*> prepare db "SELECT is_source FROM targets WHERE key=?1 LIMIT 1"
    <*> prepare db "SELECT is_errored FROM targets WHERE key=?1 LIMIT 1"
    <*> prepare db "INSERT INTO targets(key,is_errored) VALUES(?1,1) ON CONFLICT(key) DO UPDATE SET is_errored=1"
    <*> prepare db "INSERT INTO targets(key,phony) VALUES(?1,?2) ON CONFLICT(key) DO UPDATE SET phony=?2"
    <*> prepare db "SELECT phony FROM targets WHERE key=?1 LIMIT 1"

withRedoDb :: (DbConn -> IO a) -> IO a
withRedoDb action = modifyMVar cachedConn $ \mc -> do
  conn <- maybe openDbConn return mc
  result <- action conn
  return (Just conn, result)

-- | Reset all prepared statements to release any read locks, without closing the connection.
releaseRedoDbLocks :: IO ()
releaseRedoDbLocks = modifyMVar_ cachedConn $ \mc -> do
  case mc of
    Just conn -> do
      mapM_ reset
        [ stmtInitTarget conn, stmtDeleteIfChange conn, stmtDeleteIfCreate conn
        , stmtInsertIfChange conn, stmtInitSource conn, stmtExists conn
        , stmtGetStamp conn, stmtStoreStamp conn, stmtGetDoFile conn
        , stmtGetIfChangeDeps conn, stmtStoreIfChangeDep conn
        , stmtGetIfCreateDeps conn, stmtStoreIfCreateDep conn
        , stmtGetAlwaysDep conn, stmtStoreAlwaysDep conn
        , stmtGetIsSource conn, stmtGetIsErrored conn, stmtMarkErrored conn
        , stmtStorePhony conn, stmtGetPhony conn
        ]
      return (Just conn)
    Nothing -> return Nothing

closeRedoDb :: IO ()
closeRedoDb = modifyMVar_ cachedConn $ \mc -> do
  case mc of
    Just conn -> do
      -- Finalize all prepared statements before closing
      mapM_ finalize
        [ stmtInitTarget conn, stmtDeleteIfChange conn, stmtDeleteIfCreate conn
        , stmtInsertIfChange conn, stmtInitSource conn, stmtExists conn
        , stmtGetStamp conn, stmtStoreStamp conn, stmtGetDoFile conn
        , stmtGetIfChangeDeps conn, stmtStoreIfChangeDep conn
        , stmtGetIfCreateDeps conn, stmtStoreIfCreateDep conn
        , stmtGetAlwaysDep conn, stmtStoreAlwaysDep conn
        , stmtGetIsSource conn, stmtGetIsErrored conn, stmtMarkErrored conn
        , stmtStorePhony conn, stmtGetPhony conn
        ]
      close (dbHandle conn)
    Nothing -> return ()
  return Nothing

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
    , "dep TEXT NOT NULL,"
    , "UNIQUE(key, dep))"
    ]
  exec db "CREATE INDEX IF NOT EXISTS idx_ifc ON ifchange_deps(key)"
  exec db $ T.concat
    [ "CREATE TABLE IF NOT EXISTS ifcreate_deps ("
    , "key TEXT NOT NULL,"
    , "dep TEXT NOT NULL,"
    , "UNIQUE(key, dep))"
    ]
  exec db "CREATE INDEX IF NOT EXISTS idx_ifcr ON ifcreate_deps(key)"

---------------------------------------------------------------------
-- Retry logic for concurrent writes
---------------------------------------------------------------------
retryOnBusy :: IO a -> IO a
retryOnBusy action = go (50 :: Int)
  where
    go 0 = action  -- final attempt, let exception propagate
    go n = catch action $ \(e :: SQLError) ->
      case sqlError e of
        ErrorBusy         -> threadDelay 10000 >> go (n - 1)
        ErrorBusyRecovery -> threadDelay 10000 >> go (n - 1)
        ErrorBusySnapshot -> threadDelay 10000 >> go (n - 1)
        ErrorBusyTimeout  -> threadDelay 10000 >> go (n - 1)
        _                 -> throwIO e

---------------------------------------------------------------------
-- Helper: run a cached statement with params, return one row
---------------------------------------------------------------------
queryOneS :: Statement -> [SQLData] -> IO [SQLData]
queryOneS stmt params = retryOnBusy $ do
  reset stmt
  bind stmt params
  r <- step stmt
  case r of
    Row  -> columns stmt
    Done -> return []

queryAllS :: Statement -> [SQLData] -> IO [[SQLData]]
queryAllS stmt params = retryOnBusy $ do
  reset stmt
  bind stmt params
  collectRows
  where
    collectRows = do
      r <- step stmt
      case r of
        Row  -> do row <- columns stmt
                   rest <- collectRows
                   return (row : rest)
        Done -> return []

execBindS :: Statement -> [SQLData] -> IO ()
execBindS stmt params = retryOnBusy $ do
  reset stmt
  bind stmt params
  _ <- step stmt
  return ()

---------------------------------------------------------------------
-- Operations
---------------------------------------------------------------------

dbInitTarget :: DbConn -> Text -> Text -> IO ()
dbInitTarget conn key doFile = retryOnBusy $
  bracketOnError
    (exec (dbHandle conn) "BEGIN IMMEDIATE")
    (\_ -> exec (dbHandle conn) "ROLLBACK")
    (\_ -> do
      -- NOTE: preserve stamp across rebuilds (matches old filesystem behavior where stamps were separate)
      execBindS (stmtInitTarget conn) [SQLText key, SQLText doFile]
      execBindS (stmtDeleteIfChange conn) [SQLText key]
      execBindS (stmtDeleteIfCreate conn) [SQLText key]
      execBindS (stmtInsertIfChange conn) [SQLText key, SQLText doFile]
      exec (dbHandle conn) "COMMIT")

dbInitSource :: DbConn -> Text -> Text -> IO ()
dbInitSource conn key stampVal = retryOnBusy $
  bracketOnError
    (exec (dbHandle conn) "BEGIN IMMEDIATE")
    (\_ -> exec (dbHandle conn) "ROLLBACK")
    (\_ -> do
      execBindS (stmtInitSource conn) [SQLText key, SQLText stampVal]
      -- Clear stale deps if target transitioned from built to source
      execBindS (stmtDeleteIfChange conn) [SQLText key]
      execBindS (stmtDeleteIfCreate conn) [SQLText key]
      exec (dbHandle conn) "COMMIT")

dbExists :: DbConn -> Text -> IO Bool
dbExists conn key = do
  row <- queryOneS (stmtExists conn) [SQLText key]
  return $ not (null row)

dbGetStamp :: DbConn -> Text -> IO (Maybe Text)
dbGetStamp conn key = do
  row <- queryOneS (stmtGetStamp conn) [SQLText key]
  case row of
    [SQLText s] -> return (Just s)
    _           -> return Nothing

dbStoreStamp :: DbConn -> Text -> Text -> IO ()
dbStoreStamp conn key stampVal =
  execBindS (stmtStoreStamp conn) [SQLText key, SQLText stampVal]

dbGetDoFile :: DbConn -> Text -> IO (Maybe Text)
dbGetDoFile conn key = do
  row <- queryOneS (stmtGetDoFile conn) [SQLText key]
  case row of
    [SQLText s] -> return (Just s)
    _           -> return Nothing

dbGetIfChangeDeps :: DbConn -> Text -> IO [Text]
dbGetIfChangeDeps conn key = do
  rows <- queryAllS (stmtGetIfChangeDeps conn) [SQLText key]
  return [t | [SQLText t] <- rows]

dbStoreIfChangeDep :: DbConn -> Text -> Text -> IO ()
dbStoreIfChangeDep conn key dep =
  execBindS (stmtStoreIfChangeDep conn) [SQLText key, SQLText dep]

dbGetIfCreateDeps :: DbConn -> Text -> IO [Text]
dbGetIfCreateDeps conn key = do
  rows <- queryAllS (stmtGetIfCreateDeps conn) [SQLText key]
  return [t | [SQLText t] <- rows]

dbStoreIfCreateDep :: DbConn -> Text -> Text -> IO ()
dbStoreIfCreateDep conn key dep =
  execBindS (stmtStoreIfCreateDep conn) [SQLText key, SQLText dep]

dbHasAlwaysDep :: DbConn -> Text -> IO Bool
dbHasAlwaysDep conn key = do
  row <- queryOneS (stmtGetAlwaysDep conn) [SQLText key]
  case row of
    [SQLInteger 1] -> return True
    _              -> return False

dbStoreAlwaysDep :: DbConn -> Text -> IO ()
dbStoreAlwaysDep conn key =
  execBindS (stmtStoreAlwaysDep conn) [SQLText key]

dbIsSource :: DbConn -> Text -> IO Bool
dbIsSource conn key = do
  row <- queryOneS (stmtGetIsSource conn) [SQLText key]
  case row of
    [SQLInteger 1] -> return True
    _              -> return False

dbIsErrored :: DbConn -> Text -> IO Bool
dbIsErrored conn key = do
  row <- queryOneS (stmtGetIsErrored conn) [SQLText key]
  case row of
    [SQLInteger 1] -> return True
    _              -> return False

dbMarkErrored :: DbConn -> Text -> IO ()
dbMarkErrored conn key =
  execBindS (stmtMarkErrored conn) [SQLText key]

dbStorePhony :: DbConn -> Text -> Text -> IO ()
dbStorePhony conn key phony =
  execBindS (stmtStorePhony conn) [SQLText key, SQLText phony]

dbGetPhony :: DbConn -> Text -> IO (Maybe Text)
dbGetPhony conn key = do
  row <- queryOneS (stmtGetPhony conn) [SQLText key]
  case row of
    [SQLText s] -> return (Just s)
    _           -> return Nothing
