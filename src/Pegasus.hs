{-# LANGUAGE TemplateHaskell #-}

module Pegasus where

import Cardano.Api (NetworkId (..), NetworkMagic (..), SocketPath)
import Control.Monad ((>=>))
import Data.Aeson (FromJSON, object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.FileEmbed (embedFile)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Lens.Micro (at, (?~))
import Lens.Micro.Aeson (_Object)
import Pegasus.CardanoNode (CardanoNodeArgs (..), cardanoNodeProcess, defaultCardanoNodeArgs, getCardanoNodeVersion)
import Pegasus.CardanoNode.Embed (writeCardanoNodeTo)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, findExecutable, removeDirectoryRecursive)
import System.Environment (getEnv, setEnv)
import System.FilePath ((</>))
import Paths_pegasus qualified as Pkg
import System.Posix (ownerReadMode, setFileMode)
import System.Process.Typed (withProcessTerm)
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
  let p = cardanoNodeProcess dir args
  pPrint p
  withProcessTerm p $ \_p -> do
    putStrLn "Started cardano-node with:"
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
    getEnv "PATH" >>= \path -> setEnv "PATH" (binDir <> ":" <> path)

-- | Setup configuration for cardano-node to run a local devnet producing
-- blocks. This copies the appropriate files and prepares 'CardanoNodeArgs' for
-- 'withCardanoNode'.
setupCardanoDevnet :: FilePath -> IO CardanoNodeArgs
setupCardanoDevnet dir = do
  createDirectoryIfMissing True dir
  copyDevnetFiles
  refreshSystemStart
  writeTopology
  pure args
 where
  args =
    defaultCardanoNodeArgs
      { nodeDlgCertFile = Just "byron-delegation.cert"
      , nodeSignKeyFile = Just "byron-delegate.key"
      , nodeVrfKeyFile = Just "vrf.skey"
      , nodeKesKeyFile = Just "kes.skey"
      , nodeOpCertFile = Just "opcert.cert"
      }

  copyDevnetFiles = do
    BS.writeFile (dir </> nodeConfigFile args) $(embedFile "config/cardano-node.json")
    BS.writeFile (dir </> nodeByronGenesisFile args) $(embedFile "config/genesis-byron.json")
    BS.writeFile (dir </> nodeShelleyGenesisFile args) $(embedFile "config/genesis-shelley.json")
    BS.writeFile (dir </> nodeAlonzoGenesisFile args) $(embedFile "config/genesis-alonzo.json")
    BS.writeFile (dir </> nodeConwayGenesisFile args) $(embedFile "config/genesis-conway.json")
    for_ (nodeDlgCertFile args) $ \fp -> do
      BS.writeFile (dir </> fp) $(embedFile "config/byron-delegation.cert")
      setFileMode (dir </> fp) ownerReadMode
    for_ (nodeSignKeyFile args) $ \fp -> do
      BS.writeFile (dir </> fp) $(embedFile "config/byron-delegate.key")
      setFileMode (dir </> fp) ownerReadMode
    for_ (nodeVrfKeyFile args) $ \fp -> do
      BS.writeFile (dir </> fp) $(embedFile "config/vrf.skey")
      setFileMode (dir </> fp) ownerReadMode
    for_ (nodeKesKeyFile args) $ \fp -> do
      BS.writeFile (dir </> fp) $(embedFile "config/kes.skey")
      setFileMode (dir </> fp) ownerReadMode
    for_ (nodeOpCertFile args) $ \fp -> do
      BS.writeFile (dir </> fp) $(embedFile "config/opcert.cert")
      setFileMode (dir </> fp) ownerReadMode

  writeTopology =
    Aeson.encodeFile (dir </> nodeTopologyFile args) $
      object ["Producers" .= [] @String]

  -- Re-generate configuration and genesis files with fresh system start times.
  refreshSystemStart = do
    systemStart <- getCurrentTime
    let startTime = round @_ @Int $ utcTimeToPOSIXSeconds systemStart
    byronGenesis <-
      decodeJsonFileOrFail @Aeson.Value (dir </> nodeByronGenesisFile args)
        <&> atKey "startTime" ?~ toJSON startTime

    let systemStartUTC = posixSecondsToUTCTime $ realToFrac startTime
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
