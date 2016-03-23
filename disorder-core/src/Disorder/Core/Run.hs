{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
module Disorder.Core.Run (
    disorderCheckEnv
  , disorderCheckEnvWith
  , disorderCheckEnvAll
  , disorderEnvArgs
  , ExpectedTestSpeed(..)
  ) where

import           Test.QuickCheck

import           System.Environment (lookupEnv)
import           System.IO

import           Text.Read  (readMaybe)
import           Data.Maybe (fromMaybe)

import           Prelude
import           Language.Haskell.TH

data ExpectedTestSpeed
 = TestRunFast
 | TestRunNormal
 | TestRunSlow
 deriving (Eq, Ord, Show)

disorderCheckEnv :: Testable prop => ExpectedTestSpeed -> prop -> IO Result
disorderCheckEnv speed prop =
  disorderCheckEnvWith speed stdArgs prop

disorderCheckEnvWith :: Testable prop => ExpectedTestSpeed -> Args -> prop -> IO Result
disorderCheckEnvWith speed args prop = do
  args' <- disorderEnvArgs speed args
  quickCheckWithResult args' prop

disorderEnvArgs :: ExpectedTestSpeed -> Args -> IO Args
disorderEnvArgs speed args = do
  env <- readEnv $ disorderSpeedEnvArg speed
  return args { maxSuccess = fromMaybe (disorderSpeedDefaultRuns speed) env }

disorderSpeedEnvArg :: ExpectedTestSpeed -> String
disorderSpeedEnvArg =
 \case
  TestRunFast   -> "DISORDER_RUN_FAST"
  TestRunNormal -> "DISORDER_RUN_NORMAL"
  TestRunSlow   -> "DISORDER_RUN_SLOW"

disorderSpeedDefaultRuns :: ExpectedTestSpeed -> Int
disorderSpeedDefaultRuns =
 \case
  TestRunFast   -> 1000
  TestRunNormal -> 100
  TestRunSlow   -> 10


readEnv :: String -> IO (Maybe Int)
readEnv name = do
  v <- lookupEnv name
  return
    $ case v of
       Just vstr -> readMaybe vstr
       Nothing   -> Nothing


disorderCheckEnvAll :: Q Exp
disorderCheckEnvAll =
 [| \speed -> $(forAllProperties) (disorderCheckEnv speed) |]
