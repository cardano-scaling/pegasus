module Main where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString.Char8 qualified as BS8
import Data.Function ((&))
import Data.Time (NominalDiffTime)
import System.Process.Typed (createPipe, getStdout, proc, setStdout, withProcessTerm)
import System.Timeout (timeout)
import Test.HUnit (assertFailure)
import Test.Hspec (HasCallStack, Spec, hspec, it)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "starts a devnet in < 0.1 second" $ failAfter 0.1 testStartsDevnet

testStartsDevnet :: IO ()
testStartsDevnet =
  withProcessTerm cmd $ \p -> do
    waitUntilReady p
 where
  waitUntilReady p = do
    t <- BS8.hGetLine (getStdout p)
    -- TODO: update to a better "ready" indicator
    unless ("cardano-node 8.9.0" `BS8.isInfixOf` t) $
      waitUntilReady p

  cmd =
    proc "pegasus" []
      & setStdout createPipe

-- | Fail some IO action if it does not complete within given timeout.
-- A 'NominalDiffTime' can be represented as a decimal number of seconds.
failAfter :: HasCallStack => NominalDiffTime -> IO a -> IO a
failAfter seconds action =
  timeout (truncate $ seconds * 1_000_000) action >>= \case
    Nothing -> liftIO . assertFailure $ "Test timed out after " <> show seconds
    Just a -> pure a
