module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Pegasus (Devnet (..), seedDevnet, withCardanoNodeDevnet)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  -- NOTE: Allow for continous consumption of stdout
  hSetBuffering stdout NoBuffering
  withCardanoNodeDevnet "tmp-pegasus" $ \devnet -> do
    pPrint devnet
    putStrLn "Seeding wallets"
    wallets <- seedDevnet devnet 3
    pPrint wallets
    putStrLn "Producing blocks"
    forever $ do
      threadDelay . truncate $ blockTime devnet * 1_000_000
      putStr "."
