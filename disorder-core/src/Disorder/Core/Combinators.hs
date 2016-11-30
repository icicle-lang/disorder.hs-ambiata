module Disorder.Core.Combinators
  ( testJust
  , testRight
  ) where


import           Test.QuickCheck (Property, counterexample)


-- | testRight label eitherValue nextProp :
-- On a `Right`, pass the value to the next property.
-- On a `Left` fail with a label and the left value.
-- The label is useful for the case of nested `testRight`.
testRight :: Show a => String -> Either a b -> (b -> Property) -> Property
testRight _ (Right b) nextProp = nextProp b
testRight label (Left a) _ = counterexample (label ++ ": " ++ show a) False


-- | testJust label maybeValue nextProp
-- On a `Just`, pass the value to the next property.
-- On a `Nothing`, fail and print the label.
-- The label is useful for the case of nested `testJust`/`testRight``.
testJust :: String -> Maybe b -> (b -> Property) -> Property
testJust _ (Just  b) nextProp = nextProp b
testJust label _ _ = counterexample (label ++ ": Nothing ") False
