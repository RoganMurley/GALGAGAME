module GameState where

import Data.Aeson (ToJSON(..), (.=), object)

import Characters (Character(..), CharModel, FinalSelection, characterModelReverso)
import Model
import Player (WhichPlayer(..), other)
import Util (Err, Gen, shuffle, split)


data GameState =
    Waiting Gen
  | Selecting CharModel Turn Gen
  | Started PlayState
  deriving (Eq, Show)


instance ToJSON GameState where
  toJSON (Waiting _) =
    object [
      "waiting" .= True
    ]
  toJSON (Selecting m _ _) =
    toJSON m
  toJSON (Started s) =
    toJSON s


getStateGen :: GameState -> Gen
getStateGen (Waiting gen)             = gen
getStateGen (Selecting _ _ gen)       = gen
getStateGen (Started (Playing model)) = getGen model
getStateGen (Started (Ended _ gen))   = gen


data PlayState =
    Playing Model
  | Ended (Maybe WhichPlayer) Gen
  deriving (Eq, Show)


instance ToJSON PlayState where
  toJSON (Playing model) =
    object [
      "playing" .= model
    ]
  toJSON (Ended winner _) =
    object [
      "winner" .= winner
    ]


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
    f (a, b, c, d) = concat . (replicate 3) $ [a, b, c, d]


reverso :: GameState -> GameState
reverso (Waiting gen)               = Waiting gen
reverso (Selecting m t gen)         = Selecting (characterModelReverso m) t gen
reverso (Started (Playing model))   = Started . Playing $ modelReverso model
reverso (Started (Ended which gen)) = Started $ Ended (other <$> which) gen
