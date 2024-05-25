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

-- | Arguments given to the 'cardano-node' command-line to run a node.
data CardanoNodeArgs = CardanoNodeArgs
  { nodeSocket :: FilePath
  , nodeConfigFile :: FilePath
  , nodeByronGenesisFile :: FilePath
  , nodeShelleyGenesisFile :: FilePath
  , nodeAlonzoGenesisFile :: FilePath
  , nodeConwayGenesisFile :: FilePath
  , nodeTopologyFile :: FilePath
  , nodeDatabaseDir :: FilePath
  , nodeDlgCertFile :: Maybe FilePath
  , nodeSignKeyFile :: Maybe FilePath
  , nodeOpCertFile :: Maybe FilePath
  , nodeKesKeyFile :: Maybe FilePath
  , nodeVrfKeyFile :: Maybe FilePath
  }
  deriving (Show)

defaultCardanoNodeArgs :: CardanoNodeArgs
defaultCardanoNodeArgs =
  CardanoNodeArgs
    { nodeSocket = "node.socket"
    , nodeConfigFile = "cardano-node.json"
    , nodeByronGenesisFile = "genesis-byron.json"
    , nodeShelleyGenesisFile = "genesis-shelley.json"
    , nodeAlonzoGenesisFile = "genesis-alonzo.json"
    , nodeConwayGenesisFile = "genesis-conway.json"
    , nodeTopologyFile = "topology.json"
    , nodeDatabaseDir = "db"
    , nodeDlgCertFile = Nothing
    , nodeSignKeyFile = Nothing
    , nodeOpCertFile = Nothing
    , nodeKesKeyFile = Nothing
    , nodeVrfKeyFile = Nothing
    }
