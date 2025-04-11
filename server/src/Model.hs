{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Model where

import Card (Aspect (..), Card (..), Suit (..))
import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..), object, (.=))
import GHC.Generics (Generic)
import HandCard (HandCard, knownCard)
import Life (Life)
import Mirror (Mirror (..))
import Player (WhichPlayer (..), other)
import Stack (Stack, diasporaFromStack)
import StackCard (StackCard (..))
import Util (Gen)

data Model = Model
  { model_turn :: Turn,
    model_stack :: Stack,
    model_pa :: PlayerModel,
    model_pb :: PlayerModel,
    model_passes :: Passes,
    model_gen :: Gen,
    model_rot :: Int,
    model_hold :: Bool,
    model_misc :: Misc
  }
  deriving (Eq, Generic, NFData, Show)

data PlayerModel = PlayerModel
  { pmodel_hand :: Hand,
    pmodel_deck :: Deck,
    pmodel_life :: Life,
    pmodel_maxLife :: Life
  }
  deriving (Eq, Generic, NFData, Show)

type Hand = [HandCard]

type Deck = [HandCard]

type Turn = WhichPlayer

data Passes = NoPass | OnePass
  deriving (Eq, Generic, NFData, Show)

instance ToJSON Passes where
  toJSON NoPass = "NoPass"
  toJSON OnePass = "OnePass"

instance ToJSON Model where
  toJSON Model {model_turn, model_stack, model_pa, model_pb, model_rot, model_passes} =
    object
      [ "turn" .= model_turn,
        "stack" .= model_stack,
        "handPA" .= pmodel_hand model_pa,
        "handPB" .= (knownCard <$> pmodel_hand model_pb),
        "lifePA" .= pmodel_life model_pa,
        "lifePB" .= pmodel_life model_pb,
        "maxLifePA" .= pmodel_maxLife model_pa,
        "maxLifePB" .= pmodel_maxLife model_pb,
        "deckPA" .= length (pmodel_deck model_pa),
        "deckPB" .= length (pmodel_deck model_pb),
        "rot" .= model_rot,
        "pass" .= model_passes
      ]

instance Mirror Model where
  mirror m =
    Model
      { model_turn = other $ model_turn m,
        model_stack = mirror $ model_stack m,
        model_pa = model_pb m,
        model_pb = model_pa m,
        model_passes = model_passes m,
        model_gen = model_gen m,
        model_rot = model_rot m,
        model_hold = model_hold m,
        model_misc = model_misc m
      }

maxHandLength :: Int
maxHandLength = 6

getPmodel :: WhichPlayer -> Model -> PlayerModel
getPmodel PlayerA = model_pa
getPmodel PlayerB = model_pb

setPmodel :: PlayerModel -> WhichPlayer -> Model -> Model
setPmodel pmodel PlayerA model = model {model_pa = pmodel}
setPmodel pmodel PlayerB model = model {model_pb = pmodel}

modPmodel :: (PlayerModel -> PlayerModel) -> WhichPlayer -> Model -> Model
modPmodel f p m = setPmodel (f (getPmodel p m)) p m

gameover :: Model -> Bool
gameover model = (lifePA <= 0 && not invinciblePA) || (lifePB <= 0 && not invinciblePB)
  where
    lifePA = pmodel_life $ model_pa model :: Life
    lifePB = pmodel_life $ model_pb model :: Life
    invinciblePA = isInvincible PlayerA model
    invinciblePB = isInvincible PlayerB model

isInvincible :: WhichPlayer -> Model -> Bool
isInvincible w model =
  let stack = model_stack model
   in any
        ( \(_, StackCard {stackcard_card, stackcard_owner}) ->
            stackcard_owner == w
              && card_suit stackcard_card == Cup
              && card_aspect stackcard_card == Mercy
        )
        (diasporaFromStack stack)

-- Misc
data Misc = Misc
  { misc_noDrawsPa :: Int,
    misc_noDrawsPb :: Int,
    misc_forceWin :: Maybe WhichPlayer
  }
  deriving (Eq, Generic, NFData, Show)

miscInit :: Misc
miscInit =
  Misc
    { misc_noDrawsPa = 0,
      misc_noDrawsPb = 0,
      misc_forceWin = Nothing
    }

getNoDraws :: WhichPlayer -> Misc -> Int
getNoDraws PlayerA = misc_noDrawsPa
getNoDraws PlayerB = misc_noDrawsPb

incrNoDraws :: WhichPlayer -> Misc -> Misc
incrNoDraws PlayerA misc = misc {misc_noDrawsPa = misc_noDrawsPa misc + 1}
incrNoDraws PlayerB misc = misc {misc_noDrawsPb = misc_noDrawsPb misc + 1}

setForceWin :: WhichPlayer -> Misc -> Misc
setForceWin w misc = misc {misc_forceWin = Just w}
