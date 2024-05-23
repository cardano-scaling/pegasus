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
import Pegasus.CardanoNode (getCardanoNodeVersion)
import System.Directory (createDirectoryIfMissing)
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
withCardanoNodeDevnet _dir cont = do
  v <- getCardanoNodeVersion
  putStrLn $ "Using " <> v
  putStrLn "TODO: should start a devnet node"
  cont
    RunningNode
      { nodeSocket = "TODO/node.socket"
      , networkId = Testnet (NetworkMagic 11111)
      , blockTime = 20
      }
