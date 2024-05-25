{-# LANGUAGE TemplateHaskell #-}

-- | Functions to access the 'cardano-node' binary.
module Pegasus.CardanoNode.Embed where

import Data.Bits ((.|.))
import Data.ByteString qualified as BS
import Pegasus.CardanoNode.EmbedTH (embedCardanoNode)
import System.Posix.Files (ownerExecuteMode, ownerReadMode, ownerWriteMode, setFileMode)

-- | Write the embedded 'cardano-node' binary to a path.
writeCardanoNodeTo :: FilePath -> IO ()
writeCardanoNodeTo fp = do
  BS.writeFile fp $(embedCardanoNode)
  setFileMode fp (ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode)
