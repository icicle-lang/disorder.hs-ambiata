module Orphanarium.Core.Main (
    orphanariumMain
  , orphanariumCliMain
  ) where

import           Control.Applicative
import           Control.Monad

import           System.Directory
import           System.Process
import           System.Exit
import           System.IO


orphanariumMain :: [IO Bool] -> IO ()
orphanariumMain tests =
  sanity >> sequence tests >>= \rs -> unless (and rs) exitFailure

orphanariumCliMain :: [String] -> IO ()
orphanariumCliMain arguments =
  let ignore p = ".." == p || "." == p || "core" == p
      exec t = callProcess ("test/cli/" ++ t ++ "/run") arguments
   in sanity >> filter (not . ignore) <$> getDirectoryContents "test/cli/" >>= mapM_ exec

sanity :: IO ()
sanity =
  hSetBuffering stdout LineBuffering >> hSetBuffering stderr LineBuffering
