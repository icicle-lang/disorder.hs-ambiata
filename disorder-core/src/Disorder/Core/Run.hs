{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
module Disorder.Core.Run (
    disorderCheckEnv
  , disorderCheckEnvWith
  , disorderCheckEnvAll
  , disorderCheckAll
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
 = TestRunMore
 | TestRunNormal
 | TestRunFewer
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
  TestRunMore   -> "DISORDER_RUN_MORE"
  TestRunNormal -> "DISORDER_RUN_NORMAL"
  TestRunFewer  -> "DISORDER_RUN_FEWER"

disorderSpeedDefaultRuns :: ExpectedTestSpeed -> Int
disorderSpeedDefaultRuns =
 \case
  TestRunMore   -> 1000
  TestRunNormal -> 100
  TestRunFewer  -> 10


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

disorderCheckAll :: Q Exp
disorderCheckAll =
 [| $(forAllProperties) (disorderCheckEnv TestRunNormal) |]
