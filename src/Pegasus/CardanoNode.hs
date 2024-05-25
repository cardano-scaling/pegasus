module Pegasus.CardanoNode where

import Data.ByteString (toStrict)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import System.Process.Typed (proc, readProcessStdout_)

-- | Get version of 'cardano-node' from PATH.
getCardanoNodeVersion :: IO Text
getCardanoNodeVersion =
  Text.replace "\n" " "
    . Text.decodeUtf8
    . toStrict
    <$> readProcessStdout_ (proc "cardano-node" ["--version"])
