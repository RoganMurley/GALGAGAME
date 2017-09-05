{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, UnicodeSyntax #-}
module MirrorSpec where

import Data.Coerce (coerce)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)

import Characters (CharModel)
import GameState (GameState)
import Instances ()
import Mirror (Mirror, mirror)
import Model (Model, StackCard)

{-
  We use a phantom type to test typeclass instances of a given type reusably.
-}

newtype PhantomTree a = PhantomTree [TestTree]


prop :: ∀ a . (Arbitrary a, Eq a, Mirror a, Show a) => PhantomTree a
prop =
  PhantomTree [
    testProperty "twice" ((\x -> (mirror . mirror $ x) == x)  :: a -> Bool)
  ]


tests :: TestTree
tests =
  testGroup "Mirror"
    [
      testGroup "StackCard" $ coerce (prop :: PhantomTree StackCard)
    , testGroup "Model"     $ coerce (prop :: PhantomTree Model)
    , testGroup "GameState" $ coerce (prop :: PhantomTree GameState)
    , testGroup "CharModel" $ coerce (prop :: PhantomTree CharModel)
    ]
