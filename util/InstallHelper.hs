-- adding StandAloneDeriving extension:
-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- System imports:
import Control.Exception (catch, SomeException(..))
import System.Directory (removeDirectoryRecursive)

-- Local imports:
import Database

-- Main function:
main :: IO ()
main = do 
  metaDir <- redoMetaDir
  catch (removeDirectoryRecursive metaDir) (\(_ :: SomeException) -> return ())
