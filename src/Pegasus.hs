module Pegasus where

import Cardano.Api (NetworkId (..), NetworkMagic (..), SocketPath)
import Control.Monad ((>=>))
import Data.Aeson (FromJSON, Value (..), object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Functor ((<&>))
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, getCurrentTime)
import Data.Time.Clock (addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Lens.Micro (at, (?~))
import Lens.Micro.Aeson (_Object)
import Pegasus.CardanoNode (getCardanoNodeVersion, writeCardanoNodeTo)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive)
import System.Environment (getEnv, setEnv)
import System.FilePath ((</>))

data RunningNode = RunningNode
  { nodeSocket :: SocketPath
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
  createDirectoryIfMissing True binDir
  writeCardanoNodeTo $ binDir </> "cardano-node"
  -- Add devnet directory bin/ to the path
  getEnv "PATH" >>= \path -> setEnv "PATH" (path <> ":" <> binDir)
  v <- getCardanoNodeVersion
  putStrLn $ "Using " <> v
  putStrLn "TODO: should start a devnet node"
  cont
    RunningNode
      { nodeSocket = "TODO/node.socket"
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
