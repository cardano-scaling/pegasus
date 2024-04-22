module Main (main) where

import Pegasus.CardanoNodeSpec qualified
import Test.Hspec (hspec)

main :: IO ()
main = hspec Pegasus.CardanoNodeSpec.spec
