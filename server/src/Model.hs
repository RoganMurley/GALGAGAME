module Model where

import Card (Card(..))
import Data.Aeson (ToJSON(..), (.=), object)
import Life (Life)
import Mirror (Mirror(..))
import Player (WhichPlayer(..), other)
import Stack (Stack)
import Util (Gen)


data Model = Model
  { model_turn   :: Turn
  , model_stack  :: Stack
  , model_pa     :: PlayerModel
  , model_pb     :: PlayerModel
  , model_passes :: Passes
  , model_gen    :: Gen
  , model_rot    :: Int
  , model_hold   :: Bool
  }
  deriving (Eq, Show)


data PlayerModel = PlayerModel
  { pmodel_hand :: Hand
  , pmodel_deck :: Deck
  , pmodel_life :: Life
  }
  deriving (Eq, Show)


type Hand = [Card]


type Deck = [Card]


type Turn = WhichPlayer


data Passes = NoPass | OnePass
  deriving (Eq, Show)


instance ToJSON Model where
  toJSON Model{ model_turn, model_stack, model_pa, model_pb, model_rot } =
    object
      [
        "turn"   .= model_turn
      , "stack"  .= model_stack
      , "handPA" .= pmodel_hand model_pa
      , "handPB" .= length (pmodel_hand model_pb)
      , "lifePA" .= pmodel_life model_pa
      , "lifePB" .= pmodel_life model_pb
      , "rot"    .= model_rot
      ]


instance Mirror Model where
  mirror (Model turn stack pa pb passes gen rot hold) =
    Model (other turn) (mirror stack) pb pa passes gen rot hold


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
