{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ModelDiff where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson.Types (Pair, Value (Null))
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import HandCard (knownCard)
import Life (Life)
import Mirror (Mirror (..))
import Model (Deck, Hand, Misc (..), Model (..), Passes, PlayerModel (..), Turn)
import Player (WhichPlayer (..), other)
import Stack (Stack)
import Util (Gen)

data ModelDiff = ModelDiff
  { modeldiff_turn :: Maybe Turn,
    modeldiff_stack :: Maybe Stack,
    modeldiff_pa :: PlayerModelDiff,
    modeldiff_pb :: PlayerModelDiff,
    modeldiff_passes :: Maybe Passes,
    modeldiff_gen :: Maybe Gen,
    modeldiff_rot :: Maybe Int,
    modeldiff_hold :: Maybe Bool,
    modeldiff_misc :: Maybe Misc
  }
  deriving (Eq, Generic, NFData, Show)

data PlayerModelDiff = PlayerModelDiff
  { pmodeldiff_hand :: Maybe Hand,
    pmodeldiff_deck :: Maybe Deck,
    pmodeldiff_life :: Maybe Life,
    pmodeldiff_maxLife :: Maybe Life
  }
  deriving (Eq, Generic, NFData, Show)

omitNull :: [Pair] -> [Pair]
omitNull = filter ((/= Null) . snd)

instance ToJSON ModelDiff where
  toJSON ModelDiff {modeldiff_turn, modeldiff_stack, modeldiff_pa, modeldiff_pb, modeldiff_rot} =
    object . omitNull $
      [ "turn" .= modeldiff_turn,
        "stack" .= modeldiff_stack,
        "handPA" .= pmodeldiff_hand modeldiff_pa,
        "handPB" .= (fmap knownCard <$> pmodeldiff_hand modeldiff_pb),
        "lifePA" .= pmodeldiff_life modeldiff_pa,
        "lifePB" .= pmodeldiff_life modeldiff_pb,
        "maxLifePA" .= pmodeldiff_maxLife modeldiff_pa,
        "maxLifePB" .= pmodeldiff_maxLife modeldiff_pb,
        "rot" .= modeldiff_rot
      ]

instance Mirror ModelDiff where
  mirror (ModelDiff turn stack pa pb passes gen rot hold misc) =
    ModelDiff (other <$> turn) (mirror stack) pb pa passes gen rot hold misc

getPmodelDiff :: WhichPlayer -> ModelDiff -> PlayerModelDiff
getPmodelDiff PlayerA = modeldiff_pa
getPmodelDiff PlayerB = modeldiff_pb

setPmodelDiff :: PlayerModelDiff -> WhichPlayer -> ModelDiff -> ModelDiff
setPmodelDiff pmodeldiff PlayerA modeldiff = modeldiff {modeldiff_pa = pmodeldiff}
setPmodelDiff pmodeldiff PlayerB modeldiff = modeldiff {modeldiff_pb = pmodeldiff}

modPmodelDiff :: (PlayerModelDiff -> PlayerModelDiff) -> WhichPlayer -> ModelDiff -> ModelDiff
modPmodelDiff f p m = setPmodelDiff (f (getPmodelDiff p m)) p m

update :: Model -> ModelDiff -> Model
update m d =
  Model
    { model_turn = fromMaybe (model_turn m) (modeldiff_turn d),
      model_stack = fromMaybe (model_stack m) (modeldiff_stack d),
      model_passes = fromMaybe (model_passes m) (modeldiff_passes d),
      model_gen = fromMaybe (model_gen m) (modeldiff_gen d),
      model_rot = fromMaybe (model_rot m) (modeldiff_rot d),
      model_hold = fromMaybe (model_hold m) (modeldiff_hold d),
      model_pa = updateP (model_pa m) (modeldiff_pa d),
      model_pb = updateP (model_pb m) (modeldiff_pb d),
      model_misc = fromMaybe (model_misc m) (modeldiff_misc d)
    }

updateP :: PlayerModel -> PlayerModelDiff -> PlayerModel
updateP m d =
  PlayerModel
    { pmodel_hand = fromMaybe (pmodel_hand m) (pmodeldiff_hand d),
      pmodel_deck = fromMaybe (pmodel_deck m) (pmodeldiff_deck d),
      pmodel_life = fromMaybe (pmodel_life m) (pmodeldiff_life d),
      pmodel_maxLife = fromMaybe (pmodel_maxLife m) (pmodeldiff_maxLife d)
    }

instance Semigroup ModelDiff where
  a <> b =
    ModelDiff
      { modeldiff_turn = modeldiff_turn b <|> modeldiff_turn a,
        modeldiff_stack = modeldiff_stack b <|> modeldiff_stack a,
        modeldiff_passes = modeldiff_passes b <|> modeldiff_passes a,
        modeldiff_gen = modeldiff_gen b <|> modeldiff_gen a,
        modeldiff_rot = modeldiff_rot b <|> modeldiff_rot a,
        modeldiff_hold = modeldiff_hold b <|> modeldiff_hold a,
        modeldiff_pa = modeldiff_pa b <> modeldiff_pa a,
        modeldiff_pb = modeldiff_pb b <> modeldiff_pb a,
        modeldiff_misc = modeldiff_misc b <|> modeldiff_misc a
      }

instance Monoid ModelDiff where
  mappend = (<>)
  mempty =
    ModelDiff
      { modeldiff_turn = Nothing,
        modeldiff_stack = Nothing,
        modeldiff_passes = Nothing,
        modeldiff_gen = Nothing,
        modeldiff_rot = Nothing,
        modeldiff_hold = Nothing,
        modeldiff_pa = mempty,
        modeldiff_pb = mempty,
        modeldiff_misc = Nothing
      }

instance Semigroup PlayerModelDiff where
  a <> b =
    PlayerModelDiff
      { pmodeldiff_hand = pmodeldiff_hand b <|> pmodeldiff_hand a,
        pmodeldiff_deck = pmodeldiff_deck b <|> pmodeldiff_deck a,
        pmodeldiff_life = pmodeldiff_life b <|> pmodeldiff_life a,
        pmodeldiff_maxLife = pmodeldiff_maxLife b <|> pmodeldiff_maxLife a
      }

instance Monoid PlayerModelDiff where
  mappend = (<>)
  mempty = PlayerModelDiff Nothing Nothing Nothing Nothing
