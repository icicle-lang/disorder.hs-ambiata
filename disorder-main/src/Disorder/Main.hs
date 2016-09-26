module Disorder.Main (
    disorderMain
  ) where

import           Control.Concurrent.Async
import           Control.Monad

import           System.Exit
import           System.IO

import           Prelude

sequenceConcurrently :: Traversable t => t (IO a) -> IO (t a)
sequenceConcurrently = runConcurrently . sequenceA . fmap Concurrently

disorderMain :: [IO Bool] -> IO ()
disorderMain tests =
  sanity >> sequenceConcurrently tests >>= \rs -> unless (and rs) exitFailure

sanity :: IO ()
sanity =
  hSetBuffering stdout LineBuffering >> hSetBuffering stderr LineBuffering
