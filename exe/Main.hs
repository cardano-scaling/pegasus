module Main where

import Pegasus qualified

main :: IO ()
main = do
  Pegasus.startDevnetNode
  putStrLn "TODO: should seed the network"
