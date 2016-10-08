{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.Jack.Core where

import           Control.Applicative (Applicative(..))
import           Control.Monad (Monad(..), ap)

import           Data.Bool (Bool, (&&))
import           Data.Eq (Eq(..))
import           Data.Functor (Functor(..), (<$>))
import           Data.Function (($), (.))

import           Disorder.Jack.Combinators
import           Disorder.Jack.Core
import           Disorder.Jack.Property
import           Disorder.Jack.Tree

import           System.IO (IO)

import           Text.Show (Show)
import           Text.Show.Pretty (ppShow)

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Gen as QC


data Info a b =
  Info {
      infoA :: Tree a
    , infoB :: Tree b
    , infoTreeApply :: Tree (a, b)
    , infoTreeMonad :: Tree (a, b)
    , infoJackApply :: Tree (a, b)
    , infoJackMonad :: Tree (a, b)
    } deriving (Show)

genTrees :: Jack a -> Jack b -> QC.Gen (Info a b)
genTrees ja jb =
  QC.MkGen $ \r n ->
    let
      run g =
        QC.unGen g r n

      (a, b) =
        run $ do
          a0 <- runJack ja
          b0 <- runJack jb
          pure (a0, b0)
    in
      Info {
          infoA = a
        , infoB = b
        , infoTreeApply = (,) <$> a <*> b
        , infoTreeMonad = (,) `fmap` a `ap` b
        , infoJackApply = run . runJack $ (,) <$> ja <*> jb
        , infoJackMonad = run . runJack $ (,) `fmap` ja `ap` jb
        }

prop_ap_all_the_things :: Property
prop_ap_all_the_things =
  let
    aa = chooseInt (1, 5)
    bb = chooseChar ('a', 'e')
  in
    QC.forAll (genTrees aa bb) $ \(Info a b ta tm ja jm) ->
      QC.counterexample "=== A ===" .
      QC.counterexample (ppShow a) .
      QC.counterexample "=== B ===" .
      QC.counterexample (ppShow b) $
      QC.counterexample "=== Tree Applicative ===" .
      QC.counterexample (ppShow ta) $
      QC.counterexample "=== Tree Monad ===" .
      QC.counterexample (ppShow tm) $
      QC.counterexample "=== Jack Applicative ===" .
      QC.counterexample (ppShow ja) $
      QC.counterexample "=== Jack Monad ===" .
      QC.counterexample (ppShow jm) $
        ta == tm &&
        tm == ja &&
        ja == jm

return []
tests :: IO Bool
tests =
  $forAllProperties . quickCheckWithResult $ stdArgs { maxSuccess = 1000 }
