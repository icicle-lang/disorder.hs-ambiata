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

-- needs stopID for QuickCheck >= 2.10, as it now takes a Testable so false is a fail
prop_falseDoesNotFail :: Property
prop_falseDoesNotFail =
  (monadicIO . (=<<) stopIO . run) $ liftIO $ return True

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunFewer
