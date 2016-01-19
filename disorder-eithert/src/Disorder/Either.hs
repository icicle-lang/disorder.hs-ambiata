module Disorder.Either (
    testEither
  , testEitherT
  ) where

import           Control.Monad.Trans.Except (ExceptT, runExceptT)

import           Data.Text (Text, unpack)

import           Test.QuickCheck.Property


testEither :: Testable a => (e -> Text) -> Either e a -> Property
testEither t =
  either (flip counterexample False . unpack . t) property 

testEitherT :: (Functor m, Testable a) => (e -> Text) -> ExceptT e m a -> m Property
testEitherT t =
  fmap (testEither t) . runExceptT
