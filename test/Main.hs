module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (toStrict)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Function ((&))
import Data.Time (NominalDiffTime)
import Lens.Micro ((^..), (^?))
import Lens.Micro.Aeson (key, members, _Number)
import System.Process.Typed (
  ExitCode (..),
  byteStringOutput,
  createPipe,
  getStderr,
  getStdout,
  nullStream,
  proc,
  readProcessStdout_,
  readProcess_,
  runProcess_,
  setStderr,
  setStdout,
  shell,
  waitExitCode,
  withProcessTerm,
 )
import System.Timeout (timeout)
import Test.HUnit (assertFailure)
import Test.Hspec (HasCallStack, Spec, hspec, it, shouldBe, shouldReturn, shouldSatisfy)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "starts a devnet in < 1 second" testStartsDevnetWithin1Second
  it "embeds a cardano-node" testCardanoNodeEmbed
  it "stops if cardano-node dies" testCardanoNodeDies

testStartsDevnetWithin1Second :: IO ()
testStartsDevnetWithin1Second =
  withProcessTerm cmd $ \p -> do
    failAfter 1 $ waitUntilReady p
    -- Devnet should produce blocks
    b1 <- cliQueryBlock
    threadDelay 100_000 -- TODO: configurable block time
    b2 <- cliQueryBlock
    b2 `shouldSatisfy` (> b1)
    -- Devnet should contain some UTxO
    utxo <- cliQueryUTxOList
    length utxo `shouldBe` nWallets + 1
 where
  nWallets = 2

  cmd =
    proc "pegasus" ["--wallets", show nWallets]
      & setStdout createPipe
      & setStderr nullStream

  waitUntilReady p = do
    t <- BS8.hGetLine (getStdout p)
    -- TODO: update to a better "ready" indicator
    unless ("Producing blocks" `BS8.isInfixOf` t) $
      waitUntilReady p

  cliQueryBlock = do
    out <- readProcessStdout_ (shell "./tmp-pegasus/bin/cardano-cli query tip --testnet-magic 42 --socket-path tmp-pegasus/node.socket")
    pure $ out ^? key "block" . _Number

  cliQueryUTxOList = do
    out <- readProcessStdout_ (shell "./tmp-pegasus/bin/cardano-cli query utxo --whole-utxo --output-json --testnet-magic 42 --socket-path tmp-pegasus/node.socket")
    print out
    pure $ out ^.. members

testCardanoNodeEmbed :: IO ()
testCardanoNodeEmbed = do
  withProcessTerm cmd $ \_ -> do
    -- Give pegasus some time to set-up a node
    threadDelay 100_000
    void $ readProcess_ (shell "./tmp-pegasus/bin/cardano-node --version")
    void $ readProcess_ (shell "./tmp-pegasus/bin/cardano-cli --version")
 where
  cmd =
    shell "pegasus"
      & setStdout nullStream
      & setStderr nullStream

testCardanoNodeDies :: IO ()
testCardanoNodeDies = do
  withProcessTerm cmd $ \p -> do
    -- Give pegasus some time to start-up a node
    threadDelay 100_000
    runProcess_ (shell "pkill cardano-node")
    waitExitCode p `shouldReturn` ExitFailure 1
    err <- atomically (getStderr p)
    toStrict err `shouldSatisfy` BS.isInfixOf "cardano-node exited"
 where
  cmd =
    shell "pegasus"
      & setStdout nullStream
      & setStderr byteStringOutput

-- * Helpers

-- | Fail some IO action if it does not complete within given timeout.
-- A 'NominalDiffTime' can be represented as a decimal number of seconds.
failAfter :: HasCallStack => NominalDiffTime -> IO a -> IO a
failAfter seconds action =
  timeout (truncate $ seconds * 1_000_000) action >>= \case
    Nothing -> liftIO . assertFailure $ "Test timed out after " <> show seconds
    Just a -> pure a
