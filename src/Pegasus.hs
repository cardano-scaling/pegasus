module Pegasus where

import Control.Concurrent (threadDelay)

startDevnetNode :: IO ()
startDevnetNode = do
  threadDelay 50_000
  putStrLn "TODO: should start a devnet node"
