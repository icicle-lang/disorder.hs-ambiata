module Orphanarium.Core.Main ( orphanariumMain ) where

import           Control.Monad

import           System.Exit
import           System.IO


orphanariumMain :: [IO Bool] -> IO ()
orphanariumMain tests =
  hSetBuffering stdout LineBuffering >> sequence tests >>= \rs -> unless (and rs) exitFailure
