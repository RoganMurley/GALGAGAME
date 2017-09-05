{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, UnicodeSyntax #-}
module MirrorSpec where

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


unphantom :: PhantomTree a -> [TestTree]
unphantom (PhantomTree x) = x


prop :: ∀ a . (Arbitrary a, Eq a, Mirror a, Show a) => PhantomTree a
prop =
  PhantomTree [
    testProperty "twice" ((\x -> (mirror . mirror $ x) == x)  :: a -> Bool)
  ]


tests :: TestTree
tests =
  testGroup "Mirror"
    [
      testGroup "StackCard"   $ unphantom (prop :: PhantomTree StackCard)
    , testGroup "Model"       $ unphantom (prop :: PhantomTree Model)
    , testGroup "GameState"   $ unphantom (prop :: PhantomTree GameState)
    , testGroup "CharModel"   $ unphantom (prop :: PhantomTree CharModel)
    ]
