module GameState where

import Data.Aeson (ToJSON(..), (.=), object)

import Characters (Character(..), CharacterCards, CharModel, FinalSelection)
import Mirror (Mirror(..))
import Model
import Player (WhichPlayer(..), other)
import Util (Gen, shuffle, split)


data GameState =
    Waiting Gen
  | Selecting CharModel Turn Gen
  | Started PlayState
  deriving (Eq, Show)


instance ToJSON GameState where
  toJSON (Waiting _)       = object [ "waiting" .= True ]
  toJSON (Selecting m _ _) = toJSON m
  toJSON (Started s)       = toJSON s


getStateGen :: GameState -> Gen
getStateGen (Waiting gen)             = gen
getStateGen (Selecting _ _ gen)       = gen
getStateGen (Started (Playing model)) = getGen model
getStateGen (Started (Ended _ _ gen)) = gen


data PlayState =
    Playing Model
  | Ended (Maybe WhichPlayer) Model Gen
  deriving (Eq, Show)


instance ToJSON PlayState where
  toJSON (Playing model) =
    object [
      "playing" .= model
    ]
  toJSON (Ended winner model _) =
    object [
      "winner" .= winner
    , "final"  .= model
    ]

instance Mirror GameState where
  mirror (Waiting gen)             = Waiting gen
  mirror (Selecting m t gen)       = Selecting (mirror m) t gen
  mirror (Started (Playing m)) = Started . Playing . mirror $ m
  mirror (Started (Ended w m gen)) = Started $ Ended (other <$> w) (mirror m) gen


initState :: Gen -> GameState
initState = Waiting


initModel :: Turn -> FinalSelection -> FinalSelection -> Gen -> Model
initModel turn ca cb gen =
  Model turn [] pm_a pm_b NoPass gen
  where
    (genPA, genPB) = split gen :: (Gen, Gen)
    -- PlayerA
    initDeckPA = shuffle genPA (buildDeck ca) :: Deck
    (handPA, deckPA) = splitAt (initHandLength PlayerA turn) initDeckPA :: (Hand, Deck)
    pm_a = PlayerModel handPA deckPA maxLife :: PlayerModel
    -- PlayerB
    initDeckPB = shuffle genPB (buildDeck cb) :: Deck
    (handPB, deckPB) = splitAt (initHandLength PlayerB turn) initDeckPB :: (Hand, Deck)
    pm_b = PlayerModel handPB deckPB maxLife :: PlayerModel


initHandLength :: WhichPlayer -> Turn -> Int
initHandLength which first
  | which == first = maxHandLength
  | otherwise      = maxHandLength - 1


buildDeck :: FinalSelection -> Deck
buildDeck (Character _ ca, Character _ cb, Character _ cc) =
  concat $ f <$> [ca, cb, cc]
  where
    f :: CharacterCards -> [Card]
    f (a, b, c, d) = concat . (replicate 3) $ [a, b, c, d]
