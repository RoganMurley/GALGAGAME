module GameState where

import Data.Aeson (ToJSON(..), (.=), object)

import Characters (CharModel, FinalSelection, allCards, drinker)
import Life (maxLife)
import Mirror (Mirror(..))
import Model (Hand, Deck, PlayerModel(..), Model(..), Passes(..), Turn, maxHandLength)
import Player (WhichPlayer(..), other)
import Replay (Replay)
import Util (Gen, mkGen, shuffle, split)


data GameState =
    Waiting WaitType Gen
  | Selecting CharModel Turn Gen
  | Started PlayState
  deriving (Eq, Show)


instance ToJSON GameState where
  toJSON (Waiting t _)     = object [ "waiting" .= toJSON t ]
  toJSON (Selecting m _ _) = toJSON m
  toJSON (Started s)       = toJSON s


data WaitType =
    WaitCustom
  | WaitQuickplay
  deriving (Eq, Show)


instance ToJSON WaitType where
  toJSON WaitCustom    = "custom"
  toJSON WaitQuickplay = "quickplay"


getStateGen :: GameState -> Gen
getStateGen (Waiting _ gen)           = gen
getStateGen (Selecting _ _ gen)       = gen
getStateGen (Started (Playing model _)) = model_gen model
getStateGen (Started (Ended _ _ _ gen)) = gen


data PlayState =
    Playing Model Replay
  | Ended (Maybe WhichPlayer) Model Replay Gen
  deriving (Eq, Show)


instance ToJSON PlayState where
  toJSON (Playing model _) =
    object [
      "playing" .= model
    ]
  toJSON (Ended winner model _ _) =
    object [
      "winner" .= winner
    , "final"  .= model
    ]


instance Mirror PlayState where
  mirror (Playing m r)     = Playing (mirror m) (mirror r)
  mirror (Ended w m r gen) = Ended (other <$> w) (mirror m) (mirror r) gen


instance Mirror GameState where
  mirror (Waiting wait gen)  = Waiting wait gen
  mirror (Selecting m t gen) = Selecting (mirror m) t gen
  mirror (Started started)   = Started $ mirror started


initState :: WaitType -> Gen -> GameState
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


testModel :: Model
testModel = initModel PlayerA (drinker, drinker, drinker) (drinker, drinker, drinker) (mkGen 0)


initHandLength :: WhichPlayer -> Turn -> Int
initHandLength which first
  | which == first = maxHandLength
  | otherwise      = maxHandLength - 1


buildDeck :: FinalSelection -> Deck
buildDeck selection =
  allCards selection >>= replicate 3
