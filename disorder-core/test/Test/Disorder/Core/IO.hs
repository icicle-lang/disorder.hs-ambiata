{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.Core.IO where

import           Disorder.Core.IO
import           Disorder.Core.Run

import           Control.Monad.IO.Class
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

prop_falseFails :: Property
prop_falseFails
  = expectFailure $ testPropertyIO $ return False

prop_falseDoesNotFail :: Property
prop_falseDoesNotFail
  = monadicIO $ liftIO $ return False

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunFewer
