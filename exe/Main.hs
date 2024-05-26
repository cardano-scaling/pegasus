module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Pegasus (RunningNode (..), withCardanoNodeDevnet)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  -- NOTE: Allow for continous consumption of stdout
  hSetBuffering stdout NoBuffering
  withCardanoNodeDevnet "tmp-pegasus" $ \runningNode -> do
    pPrint runningNode
    putStrLn "TODO: should seed the network"
    putStrLn "Producing blocks"
    forever $ do
      threadDelay . truncate $ blockTime runningNode * 1_000_000
      putStr "."
