module Main where

import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (toStrict)
import Data.ByteString.Char8 qualified as BS8
import Data.Time (NominalDiffTime)
import System.Process.Typed (proc, readProcessStdout_)
import System.Timeout (timeout)
import Test.HUnit (assertFailure)
import Test.Hspec (HasCallStack, Spec, hspec, it, shouldSatisfy)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "starts a devnet in < 0.1 second" $
    failAfter 0.1 $ do
      out <- readProcessStdout_ (proc "pegasus" [])
      toStrict out `shouldSatisfy` ("seed the network" `BS8.isInfixOf`)

-- | Fail some IO action if it does not complete within given timeout.
-- A 'NominalDiffTime' can be represented as a decimal number of seconds.
failAfter :: HasCallStack => NominalDiffTime -> IO a -> IO a
failAfter seconds action =
  timeout (truncate $ seconds * 1_000_000) action >>= \case
    Nothing -> liftIO . assertFailure $ "Test timed out after " <> show seconds
    Just a -> pure a
