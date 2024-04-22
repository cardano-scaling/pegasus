module Pegasus.CardanoNode where

import Data.ByteString (toStrict)
import Data.ByteString.Char8 qualified as BS8
import System.Process.Typed (proc, readProcessStdout_)

-- | Get version of 'cardano-node' from PATH.
getCardanoNodeVersion :: IO String
getCardanoNodeVersion =
  BS8.unpack . toStrict
    <$> readProcessStdout_ (proc "cardano-node" ["--version"])
