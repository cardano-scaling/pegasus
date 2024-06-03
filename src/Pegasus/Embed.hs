{-# LANGUAGE TemplateHaskell #-}

-- | Functions to access embedded binaries.
module Pegasus.Embed where

import Data.Bits ((.|.))
import Data.ByteString qualified as BS
import Pegasus.EmbedTH (embedExecutable)
import System.Posix.Files (ownerExecuteMode, ownerReadMode, ownerWriteMode, setFileMode)

-- | Write the embedded 'cardano-node' binary to a path.
writeCardanoNodeTo :: FilePath -> IO ()
writeCardanoNodeTo fp = do
  BS.writeFile fp $(embedExecutable "cardano-node")
  setFileMode fp (ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode)

-- | Write the embedded 'cardano-cli' binary to a path.
writeCardanoCliTo :: FilePath -> IO ()
writeCardanoCliTo fp = do
  BS.writeFile fp $(embedExecutable "cardano-cli")
  setFileMode fp (ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode)
