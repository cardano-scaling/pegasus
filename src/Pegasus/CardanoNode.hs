{-# LANGUAGE TemplateHaskell #-}

module Pegasus.CardanoNode where

import Data.ByteString (toStrict)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Pegasus.CardanoNode.Embed (embedCardanoNode)
import System.Posix.Files (ownerExecuteMode, setFileMode)
import System.Process.Typed (proc, readProcessStdout_)

-- | Get version of 'cardano-node' from PATH.
getCardanoNodeVersion :: IO String
getCardanoNodeVersion =
  BS8.unpack . toStrict
    <$> readProcessStdout_ (proc "cardano-node" ["--version"])

-- | Write the embedded 'cardano-node' binary to a path.
writeCardanoNodeTo :: FilePath -> IO ()
writeCardanoNodeTo fp = do
  BS.writeFile fp $(embedCardanoNode)
  setFileMode fp (ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode)
