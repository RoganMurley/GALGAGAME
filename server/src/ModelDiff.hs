module ModelDiff where

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Aeson.Types (Pair, Value(Null))
import Data.Maybe (fromMaybe)
import Life (Life)
import Mirror (Mirror(..))
import Model (Hand, Deck, Model(..), Passes, PlayerModel(..), Turn, Stack)
import Player (WhichPlayer(..), other)
import Util (Gen)



data ModelDiff = ModelDiff
  { modeldiff_turn   :: Maybe Turn
  , modeldiff_stack  :: Maybe Stack
  , modeldiff_pa     :: PlayerModelDiff
  , modeldiff_pb     :: PlayerModelDiff
  , modeldiff_passes :: Maybe Passes
  , modeldiff_gen    :: Maybe Gen
  }
  deriving (Eq, Show)


data PlayerModelDiff = PlayerModelDiff
  { pmodeldiff_hand :: Maybe Hand
  , pmodeldiff_deck :: Maybe Deck
  , pmodeldiff_life :: Maybe Life
  }
  deriving (Eq, Show)


omitNull :: [Pair] -> [Pair]
omitNull = filter ((/= Null) . snd)


instance ToJSON ModelDiff where
  toJSON ModelDiff{ modeldiff_turn, modeldiff_stack, modeldiff_pa, modeldiff_pb } =
    object . (omitNull) $
      [
        "turn"   .= modeldiff_turn
      , "stack"  .= modeldiff_stack
      , "handPA" .= pmodeldiff_hand modeldiff_pa
      , "handPB" .= (length <$> pmodeldiff_hand modeldiff_pb)
      , "lifePA" .= pmodeldiff_life modeldiff_pa
      , "lifePB" .= pmodeldiff_life modeldiff_pb
      ]


instance Mirror ModelDiff where
  mirror (ModelDiff turn stack pa pb passes gen) =
    ModelDiff (other <$> turn) (mirror stack) pb pa passes gen


getPmodelDiff :: WhichPlayer -> ModelDiff -> PlayerModelDiff
getPmodelDiff PlayerA = modeldiff_pa
getPmodelDiff PlayerB = modeldiff_pb


setPmodelDiff :: PlayerModelDiff -> WhichPlayer -> ModelDiff -> ModelDiff
setPmodelDiff pmodeldiff PlayerA modeldiff = modeldiff { modeldiff_pa = pmodeldiff }
setPmodelDiff pmodeldiff PlayerB modeldiff = modeldiff { modeldiff_pb = pmodeldiff }


modPmodelDiff :: (PlayerModelDiff -> PlayerModelDiff) -> WhichPlayer -> ModelDiff -> ModelDiff
modPmodelDiff f p m = setPmodelDiff (f (getPmodelDiff p m)) p m


update :: Model -> ModelDiff -> Model
update m d =
  Model
    { model_turn   = fromMaybe (model_turn m)   (modeldiff_turn d)
    , model_stack  = fromMaybe (model_stack m)  (modeldiff_stack d)
    , model_passes = fromMaybe (model_passes m) (modeldiff_passes d)
    , model_gen    = fromMaybe (model_gen m)    (modeldiff_gen d)
    , model_pa     = updateP   (model_pa m)     (modeldiff_pa d)
    , model_pb     = updateP   (model_pb m)     (modeldiff_pb d)
    }


updateP :: PlayerModel -> PlayerModelDiff -> PlayerModel
updateP m d =
  PlayerModel
    { pmodel_hand = fromMaybe (pmodel_hand m) (pmodeldiff_hand d)
    , pmodel_deck = fromMaybe (pmodel_deck m) (pmodeldiff_deck d)
    , pmodel_life = fromMaybe (pmodel_life m) (pmodeldiff_life d)
    }


base :: ModelDiff
base =
  ModelDiff
    { modeldiff_turn   = Nothing
    , modeldiff_stack  = Nothing
    , modeldiff_passes = Nothing
    , modeldiff_gen    = Nothing
    , modeldiff_pa     = PlayerModelDiff Nothing Nothing Nothing
    , modeldiff_pb     = PlayerModelDiff Nothing Nothing Nothing
    }


merge :: ModelDiff -> ModelDiff -> ModelDiff
merge a b =
  ModelDiff
    { modeldiff_turn   = (modeldiff_turn b)   <|> (modeldiff_turn a)
    , modeldiff_stack  = (modeldiff_stack b)  <|> (modeldiff_stack a)
    , modeldiff_passes = (modeldiff_passes b) <|> (modeldiff_passes a)
    , modeldiff_gen    = (modeldiff_gen b)    <|> (modeldiff_gen a)
    , modeldiff_pa     = mergeP (modeldiff_pa b) (modeldiff_pa a)
    , modeldiff_pb     = mergeP (modeldiff_pb b) (modeldiff_pb a)
    }


mergeP :: PlayerModelDiff -> PlayerModelDiff -> PlayerModelDiff
mergeP a b =
  PlayerModelDiff
    { pmodeldiff_hand = (pmodeldiff_hand b) <|> (pmodeldiff_hand a),
      pmodeldiff_deck = (pmodeldiff_deck b) <|> (pmodeldiff_deck a),
      pmodeldiff_life = (pmodeldiff_life b) <|> (pmodeldiff_life a)
    }
