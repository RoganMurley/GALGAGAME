{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances where

import Test.QuickCheck.Instances ()
import Test.Tasty ()
import Test.Tasty.QuickCheck (Arbitrary, CoArbitrary, arbitrary, coarbitrary, elements, oneof, variant)

import System.Random (mkStdGen)

import Bounce (CardBounce(..))
import Card (Card(..))
import Cards (allCards)
import CardAnim (CardAnim(..), Hurt(..))
import DeckBuilding (Character(..), DeckBuilding(..), Rune(..))
import Discard (CardDiscard(..))
import GameState (GameState(..), PlayState(..), WaitType(..))
import Model (Model(..), PlayerModel(..), Passes(..))
import ModelDiff (ModelDiff(..), PlayerModelDiff(..))
import Player (WhichPlayer(..))
import ResolveData (ResolveData(..))
import StackCard (StackCard(..))
import Transmutation (Transmutation(..))
import Wheel (Wheel(..))

import qualified Replay.Active as Active
import qualified Replay.Final as Final
import qualified Util as Util


instance Arbitrary WhichPlayer where
  arbitrary = elements [PlayerA, PlayerB]


instance CoArbitrary WhichPlayer where
  coarbitrary PlayerA = variant (0 :: Int)
  coarbitrary PlayerB = variant (1 :: Int)


instance (Arbitrary a) => Arbitrary (Wheel a) where
  arbitrary = Wheel <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary StackCard where
  arbitrary = StackCard <$> arbitrary <*> arbitrary


instance Arbitrary Card where
  arbitrary = elements allCards


instance Arbitrary Model where
  arbitrary = Model <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance CoArbitrary Model where
  coarbitrary _ = variant (0 :: Int)


instance Arbitrary PlayerModel where
  arbitrary = PlayerModel <$> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary WaitType where
  arbitrary = elements [WaitCustom, WaitQuickplay]


instance Arbitrary Passes where
  arbitrary = elements [NoPass, OnePass]


instance Arbitrary Util.Gen where
  arbitrary = Util.Gen <$> mkStdGen <$> arbitrary


instance Arbitrary GameState where
  arbitrary = oneof
    [
      Waiting   <$> arbitrary <*> arbitrary
    , Selecting <$> arbitrary <*> arbitrary <*> arbitrary
    , Started   <$> arbitrary
    ]


instance Arbitrary PlayState where
  arbitrary = oneof
    [
      Playing <$> arbitrary <*> arbitrary
    , Ended   <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    ]


instance Arbitrary Character where
  arbitrary = Character <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary DeckBuilding where
  arbitrary = DeckBuilding <$> arbitrary <*> arbitrary


instance Arbitrary Rune where
  arbitrary = Rune <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary Active.Replay where
    arbitrary = Active.Replay <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary Final.Replay where
    arbitrary = Final.Replay <$> arbitrary <*> arbitrary


instance Arbitrary ModelDiff where
  arbitrary = ModelDiff <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary PlayerModelDiff where
  arbitrary = PlayerModelDiff <$> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary CardAnim where
  arbitrary = oneof
    [ Heal <$> arbitrary <*> arbitrary
    , Draw <$> arbitrary
    , Hurt <$> arbitrary <*> arbitrary <*> arbitrary
    , Play <$> arbitrary <*> arbitrary <*> arbitrary
    , Transmute <$> arbitrary
    , GameEnd <$> arbitrary
    , pure Rotate
    , pure Windup
    , Bounce <$> arbitrary
    , DiscardStack <$> arbitrary
    , DiscardHand <$> arbitrary <*> arbitrary
    , MoveStack <$> arbitrary
    , Pass <$> arbitrary
    ]


instance Arbitrary Hurt where
  arbitrary = oneof
    [ pure Slash
    , pure Bite
    , pure Curse
    ]


instance Arbitrary Transmutation where
  arbitrary = oneof
    [ Transmutation <$> arbitrary <*> arbitrary
    , pure NoTransmutation
    ]


instance Arbitrary CardBounce where
  arbitrary = oneof
    [ pure BounceDiscard
    , BounceIndex <$> arbitrary
    ]


instance Arbitrary CardDiscard where
  arbitrary = oneof
    [ pure CardDiscard
    , NoDiscard <$> arbitrary
    ]


instance Arbitrary ResolveData where
  arbitrary = ResolveData <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
