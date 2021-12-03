{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module GameState where

import Control.DeepSeq (NFData(..))
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import DeckBuilding (Character(..), DeckBuilding, characterCards)
import Mirror (Mirror(..))
import Model (Deck, PlayerModel(..), Model(..), Passes(..), Turn)
import Player (WhichPlayer(..), other)
import Util (Gen, shuffle, split)

import qualified Replay.Active as Active
import qualified Stack


data GameState =
    Waiting WaitType Gen
  | Selecting DeckBuilding Turn Gen
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
getStateGen (Waiting _ gen)                                  = gen
getStateGen (Selecting _ _ gen)                              = gen
getStateGen (Started (Playing (PlayingR { playing_model }))) = model_gen playing_model
getStateGen (Started (Ended _ _ _ gen))                      = gen


data PlayState =
    Playing PlayingR
  | Ended (Maybe WhichPlayer) Model Active.Replay Gen
  deriving (Eq, Generic, NFData, Show)


data PlayingR = PlayingR
  { playing_model  :: Model
  , playing_replay :: Active.Replay
  , playing_utc    :: Maybe UTCTime
  } deriving (Eq, Generic, NFData, Show)


instance ToJSON PlayState where
  toJSON (Playing (PlayingR{ playing_model })) =
    object [
      "playing" .= playing_model
    ]
  toJSON (Ended winner model _ _) =
    object [
      "winner" .= winner
    , "final"  .= model
    ]


instance Mirror PlayState where
  mirror (Playing (PlayingR m r u)) = Playing (PlayingR (mirror m) (mirror r) u)
  mirror (Ended w m r gen)          = Ended (other <$> w) (mirror m) (mirror r) gen


instance Mirror GameState where
  mirror (Waiting wait gen)  = Waiting wait gen
  mirror (Selecting m t gen) = Selecting (mirror m) t gen
  mirror (Started playState) = Started $ mirror playState


initState :: WaitType -> Gen -> GameState
initState = Waiting


initModel :: Turn -> Character -> Character -> Gen -> Model
initModel turn ca cb gen =
  Model turn Stack.init pm_a pm_b NoPass gen 0 False
  where
    (genPA, genPB) = split gen :: (Gen, Gen)
    -- PlayerA
    deckPA = shuffle genPA (buildDeck ca) :: Deck
    pm_a = PlayerModel [] deckPA (character_maxLife ca) (character_maxLife ca) :: PlayerModel
    -- PlayerB
    deckPB = shuffle genPB (buildDeck cb) :: Deck
    pm_b = PlayerModel [] deckPB (character_maxLife cb) (character_maxLife cb) :: PlayerModel


buildDeck :: Character -> Deck
buildDeck = characterCards


isWinner :: WhichPlayer -> GameState -> Bool
isWinner which (Started (Ended (Just winner) _ _ _)) = which == winner
isWinner _ _ = False


playingFromModel :: Model -> PlayingR
playingFromModel m = PlayingR m Active.null Nothing
