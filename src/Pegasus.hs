module Pegasus where

import Cardano.Api (NetworkId (..), NetworkMagic (..), SocketPath)
import Control.Monad (unless, (>=>))
import Data.Aeson (FromJSON, object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Lens.Micro (at, (?~))
import Lens.Micro.Aeson (_Object)
import Pegasus.CardanoNode (CardanoNodeArgs (..), defaultCardanoNodeArgs, getCardanoNodeVersion)
import Pegasus.CardanoNode.Embed (writeCardanoNodeTo)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable, removeDirectoryRecursive)
import System.Environment (getEnv, setEnv)
import System.FilePath ((</>))
import System.Posix (ownerReadMode, setFileMode)

import Paths_pegasus qualified as Pkg
import Text.Pretty.Simple (pPrint)

data RunningNode = RunningNode
  { nodeVersion :: Text
  , nodeSocket :: SocketPath
  , networkId :: NetworkId
  , blockTime :: NominalDiffTime
  -- ^ Expected time between blocks (varies a lot on testnets)
  }
  deriving (Show, Eq)

-- | Start a cardano devnet in given directory.
withCardanoNodeDevnet ::
  -- | Directory to persist logs and any state.
  FilePath ->
  -- | Callback when network started.
  (RunningNode -> IO a) ->
  IO a
withCardanoNodeDevnet dir cont = do
  cleanup
  instantiateCardanoNode
  findExecutable "cardano-node" >>= \case
    Nothing -> pure ()
    Just fp -> putStrLn $ "Using cardano-node: " <> fp
  nodeVersion <- getCardanoNodeVersion
  args <- setupCardanoDevnet dir
  putStrLn "TODO: should start a devnet node using"
  pPrint args
  cont
    RunningNode
      { nodeVersion
      , nodeSocket = "TODO/node.socket"
      , networkId = Testnet (NetworkMagic 11111)
      , blockTime = 20
      }
 where
  binDir = dir </> "bin"

  cleanup = do
    doesDirectoryExist dir >>= \case
      False -> pure ()
      True -> do
        putStrLn $ "Reset devnet dir " <> dir
        removeDirectoryRecursive dir

  instantiateCardanoNode = do
    createDirectoryIfMissing True binDir
    writeCardanoNodeTo $ binDir </> "cardano-node"
    -- NOTE: We put it into first position to ensure the cardano-node included
    -- is used (until users can pick one)
    getEnv "PATH" >>= \path -> setEnv "PATH" (path <> ":" <> binDir)

-- | Setup configuration for cardano-node to run a local devnet producing
-- blocks. This copies the appropriate files and prepares 'CardanoNodeArgs' for
-- 'withCardanoNode'.
setupCardanoDevnet :: FilePath -> IO CardanoNodeArgs
setupCardanoDevnet dir = do
  createDirectoryIfMissing True dir
  [dlgCert, signKey, vrfKey, kesKey, opCert] <-
    mapM
      copyDevnetCredential
      [ "byron-delegation.cert"
      , "byron-delegate.key"
      , "vrf.skey"
      , "kes.skey"
      , "opcert.cert"
      ]
  let args =
        defaultCardanoNodeArgs
          { nodeDlgCertFile = Just dlgCert
          , nodeSignKeyFile = Just signKey
          , nodeVrfKeyFile = Just vrfKey
          , nodeKesKeyFile = Just kesKey
          , nodeOpCertFile = Just opCert
          }
  copyDevnetFiles args
  refreshSystemStart args
  writeTopology args
  pure args
 where
  copyDevnetCredential file = do
    let destination = dir </> file
    exists <- doesFileExist destination
    unless exists $
      readConfigFile file >>= BS.writeFile destination
    setFileMode destination ownerReadMode
    pure destination

  copyDevnetFiles args = do
    -- TODO copy devnet files from binary
    readConfigFile "cardano-node.json"
      >>= BS.writeFile (dir </> nodeConfigFile args)
    readConfigFile "genesis-byron.json"
      >>= BS.writeFile (dir </> nodeByronGenesisFile args)
    readConfigFile "genesis-shelley.json"
      >>= BS.writeFile (dir </> nodeShelleyGenesisFile args)
    readConfigFile "genesis-alonzo.json"
      >>= BS.writeFile (dir </> nodeAlonzoGenesisFile args)
    readConfigFile "genesis-conway.json"
      >>= BS.writeFile (dir </> nodeConwayGenesisFile args)

  writeTopology args =
    Aeson.encodeFile (dir </> nodeTopologyFile args) $
      object ["Producers" .= [] @String]

  -- Re-generate configuration and genesis files with fresh system start times.
  refreshSystemStart args = do
    systemStart <- addUTCTime 1 <$> getCurrentTime

    let startTime = utcTimeToPOSIXSeconds systemStart
    byronGenesis <-
      decodeJsonFileOrFail @Aeson.Value (dir </> nodeByronGenesisFile args)
        <&> atKey "startTime" ?~ toJSON startTime

    let systemStartUTC = posixSecondsToUTCTime startTime
    shelleyGenesis <-
      decodeJsonFileOrFail @Aeson.Value (dir </> nodeShelleyGenesisFile args)
        <&> atKey "systemStart" ?~ toJSON systemStartUTC

    config <-
      decodeJsonFileOrFail @Aeson.Value (dir </> nodeConfigFile args)
        <&> (atKey "ByronGenesisFile" ?~ toJSON (Text.pack $ nodeByronGenesisFile args))
          . (atKey "ShelleyGenesisFile" ?~ toJSON (Text.pack $ nodeShelleyGenesisFile args))

    Aeson.encodeFile (dir </> nodeByronGenesisFile args) byronGenesis
    Aeson.encodeFile (dir </> nodeShelleyGenesisFile args) shelleyGenesis
    Aeson.encodeFile (dir </> nodeConfigFile args) config

  atKey i = _Object . at i

-- * Helpers

decodeJsonFileOrFail :: FromJSON a => FilePath -> IO a
decodeJsonFileOrFail = Aeson.eitherDecodeFileStrict >=> either fail pure

-- | Lookup a config file from the config/ directory (referenced via cabal data-files).
readConfigFile :: FilePath -> IO ByteString
readConfigFile fp =
  Pkg.getDataFileName ("config" </> fp) >>= BS.readFile
