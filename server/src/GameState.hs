{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module GameState where

import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Time.Clock (NominalDiffTime, UTCTime)
import DeckBuilding (Character (..), DeckBuilding, characterCards)
import GHC.Generics (Generic)
import Life (initMaxLife)
import Mirror (Mirror (..))
import Model (Deck, Model (..), Passes (..), PlayerModel (..), Turn, miscInit)
import Player (WhichPlayer (..), other)
import qualified Replay.Active as Active
import qualified Stack
import Util (Gen, shuffle, split)

data GameState
  = Waiting WaitType Gen
  | Selecting DeckBuilding Turn Gen
  | Started PlayState
  deriving (Eq, Show)

instance ToJSON GameState where
  toJSON (Waiting t _) = object ["waiting" .= toJSON t]
  toJSON (Selecting m _ _) = toJSON m
  toJSON (Started s) = toJSON s

data WaitType
  = WaitCustom
  | WaitQuickplay
  | WaitChallenge
  deriving (Eq, Show)

instance ToJSON WaitType where
  toJSON WaitCustom = "custom"
  toJSON WaitQuickplay = "quickplay"
  toJSON WaitChallenge = "challenge"

getStateGen :: GameState -> Gen
getStateGen (Waiting _ gen) = gen
getStateGen (Selecting _ _ gen) = gen
getStateGen (Started (Playing (PlayingR {playing_model}))) = model_gen playing_model
getStateGen (Started (Ended _ _ _ gen)) = gen

data PlayState
  = Playing PlayingR
  | Ended (Maybe WhichPlayer) Model Active.Replay Gen
  deriving (Eq, Generic, NFData, Show)

data PlayingR = PlayingR
  { playing_model :: Model,
    playing_replay :: Active.Replay,
    playing_utc :: Maybe UTCTime,
    playing_timeLimit :: NominalDiffTime
  }
  deriving (Eq, Generic, NFData, Show)

instance ToJSON PlayState where
  toJSON (Playing (PlayingR {playing_model})) =
    object
      [ "playing" .= playing_model
      ]
  toJSON (Ended winner model _ _) =
    object
      [ "winner" .= winner,
        "final" .= model
      ]

instance Mirror PlayState where
  mirror (Playing (PlayingR m r u t)) = Playing $ PlayingR (mirror m) (mirror r) u t
  mirror (Ended w m r gen) = Ended (other <$> w) (mirror m) (mirror r) gen

instance Mirror GameState where
  mirror (Waiting wait gen) = Waiting wait gen
  mirror (Selecting m t gen) = Selecting (mirror m) t gen
  mirror (Started playState) = Started $ mirror playState

initState :: WaitType -> Gen -> GameState
initState = Waiting

initModel :: Turn -> Maybe Character -> Maybe Character -> Gen -> Model
initModel turn ca cb gen =
  Model turn Stack.init pm_a pm_b NoPass gen 0 False miscInit
  where
    (genPA, genPB) = split gen :: (Gen, Gen)
    -- PlayerA
    deckPA = shuffle genPA (buildDeck ca) :: Deck
    lifePA = maybe initMaxLife character_maxLife ca
    pm_a = PlayerModel [] deckPA lifePA lifePA :: PlayerModel
    -- PlayerB
    deckPB = shuffle genPB (buildDeck cb) :: Deck
    lifePB = maybe initMaxLife character_maxLife cb
    pm_b = PlayerModel [] deckPB lifePB lifePB :: PlayerModel

buildDeck :: Maybe Character -> Deck
buildDeck = maybe [] characterCards

isWinner :: WhichPlayer -> GameState -> Bool
isWinner which (Started (Ended (Just winner) _ _ _)) = which == winner
isWinner _ _ = False

playingFromModel :: Model -> PlayingR
playingFromModel model =
  PlayingR
    { playing_model = model,
      playing_replay = Active.null,
      playing_utc = Nothing,
      playing_timeLimit = 0
    }

playStateFromGameState :: GameState -> Maybe PlayState
playStateFromGameState (Started started) = Just started
playStateFromGameState _ = Nothing

mapModelPlayState :: (Model -> Model) -> PlayState -> PlayState
mapModelPlayState f (Playing playing) = Playing $ playing {playing_model = f (playing_model playing)}
mapModelPlayState _ playState = playState
