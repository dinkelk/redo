{-# LANGUAGE TemplateHaskell #-}

module Version (versionString) where

import qualified System.Info
import BuildInfo (gitCommitHash, buildTimestamp)

-- Version string with platform, commit, and build time
versionString :: String
versionString = "redo 0.2.2 (" ++ System.Info.os ++ "-" ++ System.Info.arch ++ ")\n" ++
                "https://github.com/dinkelk/redo\n" ++
                "commit: " ++ $(gitCommitHash) ++ "\n" ++
                "built: " ++ $(buildTimestamp)
