module Disorder.Core.Property (
    (~~~)
  , (###)
  , (.^.)
  , (<=>)
  , (=\\=)
  , failWith
  , neg
  ) where

import           Data.AEq                 (AEq)
import qualified Data.AEq                 as AEQ
import           Data.List                ((\\))
import           Data.Text                (Text, unpack)

import           Test.QuickCheck.Gen
import           Test.QuickCheck.Property

infix 4 ~~~

-- | Approximately-equal property for floats and things that look like
-- floats.
(~~~) :: (AEq a, Show a) => a -> a -> Property
x ~~~ y = counterexample cex prop
  where
    cex  = concat ["|", show x, " - ", show y, "| > ɛ"]
    prop = x AEQ.~== y

infix 4 ###

-- | Approximately-equal property for floats, which also verifies that
-- both arguments are real numbers (i.e., not NaN or infinity).
(###) :: (AEq a, Show a, RealFloat a) => a -> a -> Property
x ### y = conjoin [counterexample unreal realProp, x ~~~ y]
  where
    unreal = unwords ["Argument is not a real number:", show x, show y]

    realProp = all real [x, y]

    real z = (not $ isNaN z) && (not $ isInfinite z)

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

infix 4 =\\=
-- |
-- Test equivalence of the lists
-- i.e. if 'ls' and 'rs' contain the same elements, possible in a different order
(=\\=) :: (Eq a, Show a) => [a] -> [a] -> Property
ls =\\= rs =
  let els = ls \\ rs
      ers = rs \\ ls
  in flip counterexample (els ++ ers == []) $
    "Lists are not equivalent: " ++
    "(ls \\\\ rs) == " ++ show els ++ " && " ++
    "(rs \\\\ ls) == " ++ show ers
