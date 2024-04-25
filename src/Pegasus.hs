module Pegasus where

import Pegasus.CardanoNode (getCardanoNodeVersion)

startDevnetNode :: IO ()
startDevnetNode = do
  v <- getCardanoNodeVersion
  putStrLn $ "Using " <> v
  putStrLn "TODO: should start a devnet node"
