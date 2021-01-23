module GameState where

import Data.Aeson (ToJSON(..), (.=), object)

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
getStateGen (Waiting _ gen)             = gen
getStateGen (Selecting _ _ gen)         = gen
getStateGen (Started (Playing model _)) = model_gen model
getStateGen (Started (Ended _ _ _ gen)) = gen


data PlayState =
    Playing Model Active.Replay
  | Ended (Maybe WhichPlayer) Model Active.Replay Gen
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
buildDeck selection =
  characterCards selection >>= replicate 3


isWinner :: WhichPlayer -> GameState -> Bool
isWinner which (Started (Ended (Just winner) _ _ _)) = which == winner
isWinner _ _ = False
