module Pegasus.CardanoNodeSpec where

import Pegasus.CardanoNode (getCardanoNodeVersion)
import Test.Hspec (Spec, it, shouldContain)

spec :: Spec
spec = do
  it "has expected cardano-node version available" $
    getCardanoNodeVersion >>= (`shouldContain` "8.9.0")
