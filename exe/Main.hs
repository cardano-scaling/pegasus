module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Pegasus qualified
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  -- NOTE: Allow for continous consumption of stdout
  hSetBuffering stdout NoBuffering
  Pegasus.withCardanoNodeDevnet "tmp-pegasus" $ \runningNode -> do
    putStrLn "Started devnet"
    pPrint runningNode
    putStrLn "TODO: should seed the network"
    forever $ do
      putStr "."
      threadDelay 100_000
