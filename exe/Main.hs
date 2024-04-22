module Main where

import Pegasus qualified

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Pegasus.someFunc
