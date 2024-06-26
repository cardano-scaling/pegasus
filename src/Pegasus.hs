{-# LANGUAGE TemplateHaskell #-}

module Pegasus where

import Cardano.Api (
  AddressInEra,
  AsType (AsGenesisUTxOKey, AsPaymentKey),
  BabbageEra,
  BuildTxWith (BuildTxWith),
  ConsensusModeParams (CardanoModeParams),
  EpochSlots (EpochSlots),
  File (File),
  FromJSON,
  GenesisUTxOKey,
  IsShelleyBasedEra (..),
  Key (
    SigningKey,
    VerificationKey,
    deterministicSigningKey,
    getVerificationKey,
    verificationKeyHash
  ),
  KeyWitnessInCtx (KeyWitnessForSpending),
  LocalNodeConnectInfo (LocalNodeConnectInfo),
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PaymentCredential (PaymentCredentialByKey),
  PaymentKey,
  SerialiseAddress (serialiseAddress),
  ShelleyWitnessSigningKey (WitnessPaymentKey),
  SocketPath,
  StakeAddressReference (NoStakeAddress),
  SubmitResult (SubmitFail, SubmitSuccess),
  Tx,
  TxFee (TxFeeExplicit),
  TxInMode (TxInMode),
  TxOut (TxOut),
  TxOutDatum (TxOutDatumNone),
  Witness (KeyWitness),
  addTxIn,
  castSigningKey,
  createAndValidateTransactionBody,
  defaultTxBodyContent,
  deterministicSigningKeySeedSize,
  genesisUTxOPseudoTxIn,
  lovelaceToTxOutValue,
  makeShelleyAddressInEra,
  setTxOuts,
  signShelleyTransaction,
  submitTxToNodeLocal,
 )
import Cardano.Crypto.Hash (SHA256, digest)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Control.Concurrent.Async (race_)
import Control.Exception (IOException, throwIO, try)
import Control.Monad (unless, (>=>))
import Data.Aeson (object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.FileEmbed (embedFile)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Lens.Micro (at, (?~))
import Lens.Micro.Aeson (_Object)
import Paths_pegasus qualified as Pkg
import Pegasus.CardanoNode (CardanoNodeArgs (..), cardanoNodeProcess, defaultCardanoNodeArgs, getCardanoNodeVersion, waitForSocket)
import Pegasus.Embed (writeCardanoCliTo, writeCardanoNodeTo)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable, removeDirectoryRecursive)
import System.Environment (getEnv, setEnv)
import System.Exit (die)
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (NoBuffering), Handle, IOMode (AppendMode), hSetBuffering)
import System.IO qualified
import System.Posix (Handler (Catch), installHandler, ownerReadMode, setFileMode, sigTERM)
import System.Process.Typed (setStdout, stopProcess, useHandleClose, waitExitCode, withProcessWait)

-- TODO: not re-exported by cardano-api nor cardano-ledger-api!?
import Cardano.Ledger.Coin (Coin (..))

-- XXX: Should be available on Cardano.Api
import Cardano.Api.Shelley (ReferenceScript (ReferenceScriptNone), serialiseToRawBytesHex, setTxFee)
import Data.Word (Word8)

data Devnet = Devnet
  { nodeVersion :: Text
  , nodeSocket :: SocketPath
  , logFile :: FilePath
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
  (Devnet -> IO ()) ->
  IO ()
withCardanoNodeDevnet dir cont = do
  cleanup
  instantiateCardanoNode
  findExecutable "cardano-node" >>= \case
    Nothing -> pure ()
    Just fp -> putStrLn $ "Using cardano-node: " <> fp
  nodeVersion <- getCardanoNodeVersion
  args@CardanoNodeArgs{nodeSocket} <- setupCardanoDevnet dir
  withLogFile logFile $ \hLog -> do
    let cmd =
          cardanoNodeProcess dir args
            & setStdout (useHandleClose hLog)
    withProcessWait cmd $ \p -> do
      -- Ensure the sub-process is also stopped when we get asked to terminate.
      _ <- installHandler sigTERM (Catch $ stopProcess p) Nothing
      race_ (waitExitCode p >>= \ec -> die $ "cardano-node exited with: " <> show ec) $ do
        let socketPath = File $ dir </> nodeSocket
        waitForSocket socketPath
        cont
          Devnet
            { nodeVersion
            , nodeSocket = socketPath
            , logFile
            , networkId = Testnet (NetworkMagic 42) -- TODO: load this from config
            , blockTime = 0.1 -- FIXME: query this
            }
 where
  binDir = dir </> "bin"

  logFile = dir </> "logs" </> "cardano-node.log"

  cleanup = do
    doesDirectoryExist dir >>= \case
      False -> pure ()
      True -> do
        putStrLn $ "Reset devnet dir " <> dir
        removeDirectoryRecursive dir

  instantiateCardanoNode = do
    createDirectoryIfMissing True binDir
    writeCardanoNodeTo $ binDir </> "cardano-node"
    -- TODO: make cli instantiation optional?
    writeCardanoCliTo $ binDir </> "cardano-cli"
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
    unlessExists (dir </> nodeConfigFile args) $ \fp ->
      BS.writeFile fp $(embedFile "config/cardano-node.json")
    unlessExists (dir </> nodeByronGenesisFile args) $ \fp ->
      BS.writeFile fp $(embedFile "config/genesis-byron.json")
    unlessExists (dir </> nodeShelleyGenesisFile args) $ \fp ->
      BS.writeFile fp $(embedFile "config/genesis-shelley.json")
    unlessExists (dir </> nodeAlonzoGenesisFile args) $ \fp ->
      BS.writeFile fp $(embedFile "config/genesis-alonzo.json")
    unlessExists (dir </> nodeConwayGenesisFile args) $ \fp ->
      BS.writeFile fp $(embedFile "config/genesis-conway.json")
    for_ (nodeDlgCertFile args) $ \fn ->
      unlessExists (dir </> fn) $ \fp -> do
        BS.writeFile fp $(embedFile "config/byron-delegation.cert")
        setFileMode fp ownerReadMode
    for_ (nodeSignKeyFile args) $ \fn ->
      unlessExists (dir </> fn) $ \fp -> do
        BS.writeFile fp $(embedFile "config/byron-delegate.key")
        setFileMode fp ownerReadMode
    for_ (nodeVrfKeyFile args) $ \fn ->
      unlessExists (dir </> fn) $ \fp -> do
        BS.writeFile fp $(embedFile "config/vrf.skey")
        setFileMode fp ownerReadMode
    for_ (nodeKesKeyFile args) $ \fn ->
      unlessExists (dir </> fn) $ \fp -> do
        BS.writeFile fp $(embedFile "config/kes.skey")
        setFileMode fp ownerReadMode
    for_ (nodeOpCertFile args) $ \fn ->
      unlessExists (dir </> fn) $ \fp -> do
        BS.writeFile fp $(embedFile "config/opcert.cert")
        setFileMode fp ownerReadMode

  unlessExists fp action = do
    exists <- doesFileExist fp
    unless exists (action fp)

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

-- | A seeded and usable wallet that will be pretty printed.
data Wallet = Wallet
  { signingKey :: SigningKey PaymentKey
  , address :: Text
  }
  deriving (Show)

-- | Distribute initial funds using a genesis transaction. That way, indexers
-- will be able to pick up the seeded funds (which is not always the case with
-- 'initialFunds' of the shelley genesis config). Each wallet is seeded with
-- 1000 ADA.
seedDevnet :: Devnet -> Word8 -> IO [Wallet]
seedDevnet devnet nWallets =
  submitTxToNodeLocal localNodeConnectInfo txInMode >>= \case
    SubmitSuccess ->
      pure wallets
    SubmitFail e ->
      die $ "Failed to submit seeding transaction: " <> show e
 where
  wallets = map (seedWallet networkId) [1 .. nWallets]

  walletAmount Wallet{signingKey} =
    (getVerificationKey signingKey, Coin 1_000_000_000)

  localNodeConnectInfo = LocalNodeConnectInfo cardanoModeParams networkId nodeSocket

  cardanoModeParams = CardanoModeParams (EpochSlots byronEpochSlots)

  byronEpochSlots = 21600

  txInMode = TxInMode shelleyBasedEra tx

  tx =
    mkGenesisTx @BabbageEra networkId faucetGenesisKey faucetAmount $
      map walletAmount wallets

  Devnet{networkId, nodeSocket} = devnet

-- | Deterministically create a wallet signing key from a number.
seedWallet :: NetworkId -> Word8 -> Wallet
seedWallet networkId seedWord =
  Wallet
    { signingKey
    , address = serialiseAddress $ mkVkAddress @BabbageEra networkId $ getVerificationKey signingKey
    }
 where
  signingKey = deterministicSigningKey AsPaymentKey $ mkSeedFromBytes seedBytes

  seedBytes = BS.replicate (fromIntegral $ deterministicSigningKeySeedSize AsPaymentKey) seedWord

-- | Create a genesis transaction using given 'initialFunds' key and amount.
-- NOTE: This function errors if the constructed transaction body is invalid.
mkGenesisTx ::
  IsShelleyBasedEra era =>
  NetworkId ->
  -- | Owner of the 'initialFunds' to use.
  SigningKey GenesisUTxOKey ->
  -- | Starting amount of 'initialFunds'.
  Coin ->
  -- | Recipients and amounts to pay in this transaction.
  [(VerificationKey PaymentKey, Coin)] ->
  Tx era
mkGenesisTx networkId genesisKey initialAmount recipients =
  case createAndValidateTransactionBody sbe body of
    Left err -> error $ "Failed to build genesis transaction: " <> show err
    Right txbody -> signShelleyTransaction sbe txbody [WitnessPaymentKey paymentKey]
 where
  sbe = shelleyBasedEra

  paymentKey = castSigningKey genesisKey

  body =
    defaultTxBodyContent sbe
      & addTxIn (initialInput, BuildTxWith $ KeyWitness KeyWitnessForSpending)
      & setTxOuts (recipientOutputs <> [changeOutput])
      & setTxFee (TxFeeExplicit sbe fee)

  -- NOTE: Slighly over-pay to not need full fee calculation using parameters
  fee = Coin 170000

  initialInput =
    genesisUTxOPseudoTxIn
      networkId
      (verificationKeyHash $ getVerificationKey genesisKey)

  totalSent = foldMap snd recipients

  changeAddr = mkVkAddress networkId (getVerificationKey paymentKey)

  changeOutput =
    TxOut
      changeAddr
      (lovelaceToTxOutValue sbe $ initialAmount - totalSent - fee)
      TxOutDatumNone
      ReferenceScriptNone

  recipientOutputs =
    flip map recipients $ \(vk, ll) ->
      TxOut
        (mkVkAddress networkId vk)
        (lovelaceToTxOutValue sbe ll)
        TxOutDatumNone
        ReferenceScriptNone

-- | Get the unstaked address of given verification key on given network.
mkVkAddress :: IsShelleyBasedEra era => NetworkId -> VerificationKey PaymentKey -> AddressInEra era
mkVkAddress networkId vk =
  makeShelleyAddressInEra
    shelleyBasedEra
    networkId
    (PaymentCredentialByKey $ verificationKeyHash vk)
    NoStakeAddress

-- * Fixtures

faucetGenesisKey :: SigningKey GenesisUTxOKey
faucetGenesisKey =
  deterministicSigningKey AsGenesisUTxOKey seed
 where
  seed = mkSeedFromBytes $ digest (Proxy @SHA256) "faucet"

-- NOTE: Needs to match initialFunds in genesis-shelley.json
faucetAddress :: ByteString
faucetAddress =
  serialiseToRawBytesHex $
    makeShelleyAddressInEra
      (shelleyBasedEra @BabbageEra)
      (Testnet (NetworkMagic 42))
      (PaymentCredentialByKey $ verificationKeyHash $ getVerificationKey $ castSigningKey faucetGenesisKey)
      NoStakeAddress

-- NOTE: Needs to match initialFunds in genesis-shelley.json
faucetAmount :: Coin
faucetAmount = 900000000000

-- * Helpers

decodeJsonFileOrFail :: FromJSON a => FilePath -> IO a
decodeJsonFileOrFail = Aeson.eitherDecodeFileStrict >=> either fail pure

-- | Lookup a config file from the config/ directory (referenced via cabal data-files).
readConfigFile :: FilePath -> IO ByteString
readConfigFile fp =
  Pkg.getDataFileName ("config" </> fp) >>= BS.readFile

-- | Open a non-buffered log file in append mode.
withLogFile :: FilePath -> (Handle -> IO b) -> IO b
withLogFile fp action = do
  createDirectoryIfMissing True (takeDirectory fp)
  withFile fp AppendMode (\out -> hSetBuffering out NoBuffering >> action out)

-- | Like 'withFile' from 'base', but without annotating errors originating from
-- enclosed action.
--
-- XXX: This should be fixed upstream in 'base'.
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile fp mode action =
  System.IO.withFile fp mode (try . action) >>= \case
    Left (e :: IOException) -> throwIO e
    Right x -> pure x
