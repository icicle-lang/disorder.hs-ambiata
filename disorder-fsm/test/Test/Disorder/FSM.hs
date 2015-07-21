module Test.Disorder.FSM where

import           Control.Applicative

import qualified Test.Disorder.FSM.IO
import qualified Test.Disorder.FSM.Cont


import           Prelude
tests :: IO Bool
tests = and <$> sequence [
      Test.Disorder.FSM.IO.tests
    , Test.Disorder.FSM.Cont.tests
    ]
