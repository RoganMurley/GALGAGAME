{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances where

import Test.QuickCheck.Instances ()
import Test.Tasty ()
import Test.Tasty.QuickCheck (Arbitrary, CoArbitrary, arbitrary, coarbitrary, elements, oneof, variant)

import System.Random (mkStdGen)

import Characters (Character(..), CharModel(..), SelectedCharacters(..))
import GameState (GameState(..), PlayState(..))
import Model (Card(..), Model(..), PlayerModel(..), Passes(..), StackCard(..))
import Player (WhichPlayer(..))

import qualified Util as Util


instance Arbitrary WhichPlayer where
  arbitrary = elements [PlayerA, PlayerB]


instance CoArbitrary WhichPlayer where
  coarbitrary PlayerA = variant (0 :: Int)
  coarbitrary PlayerB = variant (1 :: Int)


instance Arbitrary StackCard where
  arbitrary = StackCard <$> arbitrary <*> arbitrary


instance Arbitrary Card where
  arbitrary = Card <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary Model where
  arbitrary = Model <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance CoArbitrary Model where
  coarbitrary _ = variant (0 :: Int)


instance Arbitrary PlayerModel where
  arbitrary = PlayerModel <$> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary Passes where
  arbitrary = elements [NoPass, OnePass]


instance Arbitrary Util.Gen where
  arbitrary = Util.Gen <$> mkStdGen <$> arbitrary


instance Arbitrary GameState where
  arbitrary = oneof
    [
      Waiting   <$> arbitrary
    , Selecting <$> arbitrary <*> arbitrary <*> arbitrary
    , Started   <$> arbitrary
    ]


instance Arbitrary PlayState where
  arbitrary = oneof
    [
      Playing <$> arbitrary
    , Ended   <$> arbitrary <*> arbitrary
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
