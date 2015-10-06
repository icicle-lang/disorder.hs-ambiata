{-# LANGUAGE OverloadedStrings #-}

module Disorder.Cli.Shell (
    testShell
  , testShell'
  , cliOutput
  ) where

import qualified Data.Text           as T

import           Turtle

-- | We could just use execve rather than building a string to pass to a
-- shell, but we want to simulate an interactive shell/shell script as
-- as closely as possible.
--
-- Of course, as this function directly passes its input to a shell, it should
-- never be used for anything except a test with no uncontrolled input.
testShell :: [Text] -> Shell Text -> IO (ExitCode, Text)
testShell args = shellStrict args'
  where
    args' = T.intercalate " " args

testShell' :: [Text] -> IO (ExitCode, Text)
testShell' = flip testShell empty

-- | Render the 'Text' value expected from a line of output.
cliOutput :: (Show a) => a -> Text
cliOutput = flip T.snoc '\n' . T.pack . show
