{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Model where

import Card (Card(..))
import Control.DeepSeq (NFData(..))
import Data.Aeson (ToJSON(..), (.=), object)
import GHC.Generics (Generic)
import HandCard (HandCard, anyCard, knownCard)
import Life (Life)
import Mirror (Mirror(..))
import Player (WhichPlayer(..), other)
import Stack (Stack)
import Util (Gen)


data Model = Model
  { model_turn    :: Turn
  , model_stack   :: Stack
  , model_pa      :: PlayerModel
  , model_pb      :: PlayerModel
  , model_passes  :: Passes
  , model_gen     :: Gen
  , model_rot     :: Int
  , model_hold    :: Bool
  }
  deriving (Eq, Generic, NFData, Show)


data PlayerModel = PlayerModel
  { pmodel_hand    :: Hand
  , pmodel_deck    :: Deck
  , pmodel_life    :: Life
  , pmodel_maxLife :: Life
  }
  deriving (Eq, Generic, NFData, Show)


type Hand = [HandCard]


type Deck = [Card]


type Turn = WhichPlayer


data Passes = NoPass | OnePass
  deriving (Eq, Generic, NFData, Show)


instance ToJSON Model where
  toJSON Model{ model_turn, model_stack, model_pa, model_pb, model_rot } =
    object
      [
        "turn"      .= model_turn
      , "stack"     .= model_stack
      , "handPA"    .= (anyCard <$> pmodel_hand model_pa)
      , "handPB"    .= (knownCard <$> pmodel_hand model_pb)
      , "lifePA"    .= pmodel_life model_pa
      , "lifePB"    .= pmodel_life model_pb
      , "maxLifePA" .= pmodel_maxLife model_pa
      , "maxLifePB" .= pmodel_maxLife model_pb
      , "deckPA"    .= length (pmodel_deck model_pa)
      , "deckPB"    .= length (pmodel_deck model_pb)
      , "rot"       .= model_rot
      ]


instance Mirror Model where
  mirror m =
    Model
    { model_turn    = other $ model_turn m
    , model_stack   = mirror $ model_stack m
    , model_pa      = model_pb m
    , model_pb      = model_pa m
    , model_passes  = model_passes m
    , model_gen     = model_gen m
    , model_rot     = model_rot m
    , model_hold    = model_hold m
    }


maxHandLength :: Int
maxHandLength = 6


getPmodel :: WhichPlayer -> Model -> PlayerModel
getPmodel PlayerA = model_pa
getPmodel PlayerB = model_pb


setPmodel :: PlayerModel -> WhichPlayer -> Model -> Model
setPmodel pmodel PlayerA model = model { model_pa = pmodel }
setPmodel pmodel PlayerB model = model { model_pb = pmodel }


modPmodel :: (PlayerModel -> PlayerModel) -> WhichPlayer -> Model -> Model
modPmodel f p m = setPmodel (f (getPmodel p m)) p m


gameover :: Model -> Bool
gameover model = lifePA <= 0 || lifePB <= 0
  where
    lifePA = pmodel_life $ model_pa model :: Life
    lifePB = pmodel_life $ model_pb model :: Life
