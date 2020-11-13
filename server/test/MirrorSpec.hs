{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, UnicodeSyntax #-}
module MirrorSpec where

import Data.Coerce (coerce)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)

import CardAnim (CardAnim)
import DeckBuilding (DeckBuilding)
import GameState (GameState, PlayState)
import Instances ()
import Mirror (Mirror, mirror)
import Model (Model)
import ModelDiff (ModelDiff)
import StackCard (StackCard)
import Stack (Stack)
import Transmutation (Transmutation)

import qualified Replay.Active as Active
import qualified Replay.Final as Final

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
    [ testGroup "StackCard"     $ coerce (prop :: PhantomTree StackCard)
    , testGroup "Model"         $ coerce (prop :: PhantomTree Model)
    , testGroup "GameState"     $ coerce (prop :: PhantomTree GameState)
    , testGroup "PlayState"     $ coerce (prop :: PhantomTree PlayState)
    , testGroup "DeckBuilding"  $ coerce (prop :: PhantomTree DeckBuilding)
    , testGroup "CardAnim"      $ coerce (prop :: PhantomTree CardAnim)
    , testGroup "Active.Replay" $ coerce (prop :: PhantomTree Active.Replay)
    , testGroup "Final.Replay"  $ coerce (prop :: PhantomTree Final.Replay)
    , testGroup "ModelDiff"     $ coerce (prop :: PhantomTree ModelDiff)
    , testGroup "Stack"         $ coerce (prop :: PhantomTree Stack)
    , testGroup "Transmutation" $ coerce (prop :: PhantomTree Transmutation)
    ]
