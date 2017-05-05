module GameState where

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Safe (headMay, tailSafe)
import Data.Text (Text)

import Characters
import Model
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


data GameCommand =
    EndTurn
  | PlayCard Int
  | HoverCard (Maybe Int)
  | Rematch
  | SelectCharacter Text
  | Chat Username Text
  deriving (Show)


initState :: Gen -> GameState
initState = Waiting


initModel :: Turn -> FinalSelection -> FinalSelection -> Gen -> Model
initModel turn ca cb gen =
  Model
    turn
    []
    (PlayerModel
      handPA
      deckPA
      maxLife)
    (PlayerModel
      handPB
      deckPB
      maxLife)
    NoPass
    gen
  where
    (genPA, genPB) = split gen :: (Gen, Gen)
    initDeckPA = shuffle (buildDeck ca) genPA :: Deck
    (handPA, deckPA) = splitAt 5 initDeckPA :: (Hand, Deck)
    initDeckPB = shuffle (buildDeck cb) genPB :: Deck
    (handPB, deckPB) = splitAt 5 initDeckPB :: (Hand, Deck)


buildDeck :: FinalSelection -> Deck
buildDeck (Character _ _ cards1, Character _ _ cards2, Character _ _ cards3) =
  (f cards1) ++ (f cards2) ++ (f cards3)
  where
    f (a, b, c, d) = concat . (replicate 3) $ [a, b, c, d]


reverso :: GameState -> GameState
reverso (Waiting gen)               = Waiting gen
reverso (Selecting m t gen)         = Selecting (characterModelReverso m) t gen
reverso (Started (Playing model))   = Started . Playing $ modelReverso model
reverso (Started (Ended which gen)) = Started $ Ended (other <$> which) gen


update :: GameCommand -> WhichPlayer -> GameState -> Either Err (Maybe GameState, [Outcome])
update (Chat username msg) _ _ = Right (Nothing, [EncodableOutcome $ ChatOutcome username msg])
update cmd which state =
  case state of
    Waiting _ ->
      Left ("Unknown command " <> (cs $ show cmd) <> " on a waiting GameState")
    Selecting selectModel turn gen ->
      case cmd of
        SelectCharacter n ->
          let
            startIfBothReady :: GameState -> GameState
            startIfBothReady (Selecting (CharModel (ThreeSelected c1 c2 c3) (ThreeSelected ca cb cc) _) _ _) =
              Started . Playing $ initModel turn (c1, c2, c3) (ca, cb, cc) gen
            startIfBothReady s = s
          in
            Right (Just . startIfBothReady $ Selecting (selectChar selectModel which n) turn gen, [SyncOutcome])
        _ ->
          Left ("Unknown command " <> (cs $ show cmd) <> " on a selecting GameState")
    Started started ->
      case started of
        Playing model ->
          case cmd of
            EndTurn ->
              endTurn which model
            PlayCard index ->
              ((\x -> (x, [SyncOutcome, PlayCardOutcome which])) . Just . Started . Playing) <$> (playCard index which model)
            HoverCard index ->
              (\x -> (Nothing, [x])) <$> (hoverCard index which model)
            _ ->
              Left ("Unknown command " <> (cs $ show cmd) <> " on a Playing GameState")
        Ended winner gen ->
          case cmd of
            Rematch ->
              Right . (\x -> (x, [SyncOutcome])) . Just $ Selecting initCharModel (fromMaybe PlayerA winner) (fst $ split gen)
            _ ->
              Left ("Unknown command " <> (cs $ show cmd) <> " on an Ended GameState")


endTurn :: WhichPlayer -> Model -> Either Err (Maybe GameState, [Outcome])
endTurn which model
  | turn /= which = Left "You can't end the turn when it's not your turn!"
  | handFull =  Left "You can't end the turn when your hand is full!"
  | otherwise =
    case passes of
      OnePass ->
        case runWriter . resolveAll $ model of
          (Playing m, res) ->
            let newState = Started . Playing . drawCards . resetPasses . swapTurn $ m in
              Right (Just newState, [EncodableOutcome $ ResolveOutcome res newState, EndTurnOutcome which])
          (Ended w g, res) ->
            let newState = Started (Ended w g) in
              Right (Just newState, [EncodableOutcome $ ResolveOutcome res newState, EndTurnOutcome which])
      NoPass ->
        Right (Just . Started . Playing . swapTurn $ model, [SyncOutcome])
  where
    turn = getTurn model :: Turn
    passes = getPasses model :: Passes
    handFull = length (getHand which model) >= maxHandLength :: Bool
    drawCards :: Model -> Model
    drawCards m = (drawCard PlayerA) . (drawCard PlayerB) $ m


resolveAll :: Model -> Writer [Model] PlayState
resolveAll model
  | null stack = return (Playing model)
  | otherwise =
    do
      tell [model]
      case resolveOne model of
        Playing newModel ->
          resolveAll newModel
        Ended which gen ->
          return (Ended which gen)
  where
    stack = getStack model :: Stack
    resolveOne :: Model -> PlayState
    resolveOne m =
      lifeGate . eff $ modStack tailSafe m
      where
        eff :: Model -> Model
        eff =
          case headMay stack of
            Nothing ->
              id
            Just (StackCard o c) ->
              (card_eff c) o

lifeGate :: Model -> PlayState
lifeGate m
  | lifePA <= 0 && lifePB <= 0 =
    Ended Nothing gen
  | lifePB <= 0 =
    Ended (Just PlayerA) gen
  | lifePA <= 0 =
    Ended (Just PlayerB) gen
  | otherwise =
    Playing
      . (setLife PlayerA (min maxLife lifePA))
      . (setLife PlayerB (min maxLife lifePB))
      $ m
  where
    gen = getGen m :: Gen
    lifePA = getLife PlayerA m :: Life
    lifePB = getLife PlayerB m :: Life


type ExcludePlayer = WhichPlayer
type Username = Text


-- OUTCOMES
data Outcome =
    SyncOutcome
  | PlayCardOutcome ExcludePlayer
  | EndTurnOutcome ExcludePlayer
  | EncodableOutcome EncodableOutcome
  deriving (Eq, Show)


data EncodableOutcome =
    ChatOutcome Username Text
  | HoverOutcome ExcludePlayer (Maybe Int)
  | ResolveOutcome [Model] GameState
  deriving (Eq, Show)


instance ToJSON EncodableOutcome where
  toJSON (ChatOutcome name msg) =
    object [
      "name" .= name
    , "msg"  .= msg
    ]
  toJSON (HoverOutcome _ index) =
    toJSON index
  toJSON (ResolveOutcome res state) =
    object [
      "list" .= res
    , "final" .= state
    ]


hoverCard :: Maybe Int -> WhichPlayer -> Model -> Either Err Outcome
hoverCard index which model =
  case index of
    Just i ->
      if i < (length . (getHand which) $ model)
        then
          Right . EncodableOutcome $ HoverOutcome which (Just i)
        else
          Left ("Hover index out of bounds (" <> (cs . show $ i ) <> ")" :: Err)
    Nothing ->
      Right . EncodableOutcome $ HoverOutcome which Nothing
