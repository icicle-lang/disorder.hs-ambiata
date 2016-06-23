jack
====

```
Jack's love of dice has brought him here, where he has taken on the form
of a Haskell library, in order to help you gamble with your properties.
```

![](img/dice.jpg)

Jack is an alternative to QuickCheck generators / shrinking. The basic
idea is that instead of generating a random value and using a shrinking
function after the fact, we generate the random value and all the
possible shrinks in a tree.

## Why Jack?

Generating the shrinks when you generate your initial value has many
useful properties, you can easily maintain the invariants of the
generator for example. In QuickCheck if you do `choose (100, 200)` and
then try to shrink it, it will happily shrink to `0`.

QuickCheck shrinking functions are also invariant, so if you have
a shrinker for `Text` you cannot lift it to a shrinker of `Foo` without
having a mapping in both directions. This breaks the beautiful
applicative syntax that generators can be constructed with. Jack doesn't
have this problem, a `Jack Text` can be turned in to a `Jack Foo` using
only `fmap`, and your `Foo` will be shrunk for free.

## Generators

Here's an example usage of Jack for building up generators. It works
much the same as building up QuickCheck generators except you get
shrinking of sub-terms for free. You can use the `reshrink` function to
apply any additional shrinking:

```hs
data Exp =
    Con !Int
  | Var !Text
  | Lam !Text !Exp
  | App !Exp !Exp
    deriving (Eq, Ord, Show)

exp :: Jack Exp
exp =
  let
    text =
      T.pack <$> arbitrary

    shrink = \case
      Lam _ x ->
        [x]
      App x y ->
        [x, y]
      _ ->
        []
  in
    reshrink shrink $
      oneOfRec [
          Con <$> sizedIntegral
        , Var <$> text
        ] [
          Lam <$> text <*> exp
        , App <$> exp <*> exp
        ]
```

## Properties

To use a Jack generator in a property test pass it to the `gamble`
function, this is the equivalent to `forAll` in QuickCheck:

```hs
prop_example :: Property
prop_example
  gamble exp allJack

-- | Ensure all variable names start with jack.
allJack :: Exp -> Bool
allJack = \case
  Con _ ->
    True
  Var var ->
    "jack" `T.isPrefixOf` var
  Lam var x ->
    "jack" `T.isPrefixOf` var && allJack x
  App x1 x2 ->
    allJack x1 x2
```

## QuickCheck Compatibility

Jack has a compatibility module which makes it a drop-in replacement for
QuickCheck in many cases. Just import `Test.QuickCheck.Jack` instead of
`Test.QuickCheck` and you should get shrinking for free as long as you
are using `forAll` instead of `Arbitrary` instances.

`Test.QuickCheck.Jack` essentially aliases `Jack` to `Gen` and `gamble`
to `forAll`. Unfortunately you'll need qualified imports if you still
want to use `Arbitrary` instances, however you won't want to do this
anyway as they defeat shrinking for the most part.
