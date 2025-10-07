module BuildInfo (gitCommitHash, buildTimestamp) where

import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Language.Haskell.TH
import System.Process (readProcess)

-- Get git commit hash at compile time
gitCommitHash :: Q Exp
gitCommitHash = do
  hash <- runIO $ readProcess "git" ["rev-parse", "--short", "HEAD"] ""
  stringE (init hash)  -- remove trailing newline

-- Get build timestamp at compile time
buildTimestamp :: Q Exp
buildTimestamp = do
  now <- runIO getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" now
  stringE timestamp
