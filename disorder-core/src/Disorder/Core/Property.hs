module Disorder.Core.Property where

import           Data.AEq                 (AEq)
import qualified Data.AEq                 as AEQ
import           Data.Text                (Text, unpack)

import           Test.QuickCheck.Gen
import           Test.QuickCheck.Property


infix 4 =/=
(=/=) :: (Eq a, Show a) => a -> a -> Property
x =/= y = counterexample (concat [show x, " == ", show y]) $ x /= y

infix 4 ~~~

-- | Approximately-equal property for floats and things that look like
-- floats.
(~~~) :: (AEq a, Show a) => a -> a -> Property
x ~~~ y = counterexample cex prop
  where
    cex  = concat ["|", show x, " - ", show y, "| > ɛ"]
    prop = x AEQ.~== y

failWith :: Text -> Property
failWith =
  flip counterexample False . unpack

-- |
-- Allows you to negate a property and provide a string to hopefully give some clue as to what
-- went wrong.
--
neg :: (Testable p) => p -> Property
neg x =
    let
        genRose :: (Testable p) => p -> Gen (Rose Result)
        genRose = fmap unProp . unProperty . property

        checkExpectFailure :: Result -> Rose Result -> Rose Result
        checkExpectFailure res rose =
            if expect res then
                rose
            else
                return $ failed { reason = "expectFailure may not occur inside a negation" }

        negRose :: Rose Result -> Rose Result
        negRose rose = do
            res <- rose
            checkExpectFailure res . return $ case ok res of
                Nothing -> res
                Just b  -> res { ok = Just $ not b }


    -- Sorry cant think of a more helpful thing to say... Can't change what will get printed by the negated
    -- property in a meaningful way.. can only append a "not" to it...
    --
    in counterexample "The following properties are ALL true.... NOT!:" . MkProperty $ do
        rose <- genRose x
        return . MkProp $ negRose rose

infixr 1 .^.
(.^.) :: (Testable p1, Testable p2) => p1 -> p2 -> Property
p1 .^. p2 = (p1 .||. p2) .&&. neg (p1 .&&. p2)

infixr 1 <=>
(<=>) :: (Testable p1, Testable p2) => p1 -> p2 -> Property
a <=> b = (a .&&. b) .||. (neg a .&&. neg b)
