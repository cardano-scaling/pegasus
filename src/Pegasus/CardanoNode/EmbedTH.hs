-- | Template haskell expression to embed the 'cardano-node'.
module Pegasus.CardanoNode.EmbedTH where

import Data.FileEmbed (embedFile)
import Language.Haskell.TH (Exp, Q, runIO)
import System.Directory (findExecutable)

-- | Template haskell expression to find and embed the 'cardano-node' binary.
embedCardanoNode :: Q Exp
embedCardanoNode = do
  fp <- runIO $ do
    findExecutable "cardano-node" >>= \case
      Nothing -> fail "cardano-node not found, ensure it is in PATH when compiling (and do a cabal clean)"
      Just fp -> do
        putStrLn $ "Embedding cardano-node from: " <> fp
        pure fp
  embedFile fp
