{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances where

import Test.QuickCheck.Instances ()
import Test.Tasty ()
import Test.Tasty.QuickCheck (Arbitrary, CoArbitrary, arbitrary, coarbitrary, elements, oneof, variant)

import System.Random (mkStdGen)

import Card (Card(..))
import Cards (allCards)
import CardAnim (CardAnim(..))
import Characters (Character(..), CharModel(..), SelectedCharacters(..))
import GameState (GameState(..), PlayState(..), WaitType(..))
import Model (Model(..), PlayerModel(..), Passes(..))
import ModelDiff (ModelDiff(..), PlayerModelDiff(..))
import Player (WhichPlayer(..))
import StackCard (StackCard(..))
import Username (Username(Username))

import qualified Replay.Active as Active
import qualified Replay.Final as Final
import qualified Util as Util


instance Arbitrary WhichPlayer where
  arbitrary = elements [PlayerA, PlayerB]


instance CoArbitrary WhichPlayer where
  coarbitrary PlayerA = variant (0 :: Int)
  coarbitrary PlayerB = variant (1 :: Int)


instance Arbitrary StackCard where
  arbitrary = StackCard <$> arbitrary <*> arbitrary


instance Arbitrary Card where
  arbitrary = elements allCards


instance Arbitrary Model where
  arbitrary = Model <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


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


instance Arbitrary CharModel where
  arbitrary = CharModel <$> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary Character where
  arbitrary = Character <$> arbitrary <*> arbitrary


instance Arbitrary SelectedCharacters where
  arbitrary = oneof
    [
      pure NoneSelected
    , OneSelected   <$> arbitrary
    , TwoSelected   <$> arbitrary <*> arbitrary
    , ThreeSelected <$> arbitrary <*> arbitrary <*> arbitrary
    ]


instance Arbitrary Active.Replay where
    arbitrary = Active.Replay <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary Final.Replay where
    arbitrary = Final.Replay <$> arbitrary <*> arbitrary


instance Arbitrary Username where
    arbitrary = Username <$> arbitrary


instance Arbitrary ModelDiff where
  arbitrary = ModelDiff <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary PlayerModelDiff where
  arbitrary = PlayerModelDiff <$> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary CardAnim where
  arbitrary = oneof
    [ Slash <$> arbitrary <*> arbitrary
    , Heal <$> arbitrary
    , Draw <$> arbitrary
    , Bite <$> arbitrary <*> arbitrary
    , pure Reflect
    , pure Reverse
    , pure Hubris
    , Play <$> arbitrary <*> arbitrary <*> arbitrary
    , Transmute <$> arbitrary <*> arbitrary
    , GameEnd <$> arbitrary
    ]
