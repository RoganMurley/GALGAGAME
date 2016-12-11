module GameState where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Safe (headMay, tailSafe)
import Data.Text (Text)

import Cards
import Model
import Util (Err, Gen, shuffle, split)


data GameState =
    Waiting Gen
  | Started PlayState
  deriving (Eq, Show)


instance ToJSON GameState where
  toJSON (Waiting _) =
    object [
      "waiting" .= True
    ]
  toJSON (Started s) =
    toJSON s


data PlayState =
    Playing Model
  | Ended (Maybe WhichPlayer) Gen ResolveList
  deriving (Eq, Show)


instance ToJSON PlayState where
  toJSON (Playing model) =
    object [
      "playing" .= toJSON model
    ]
  toJSON (Ended winner _ res) =
    object [
      "winner" .= winner
    , "res"    .= res
    ]


data GameCommand =
    EndTurn
  | PlayCard CardName
  | HoverCard (Maybe CardName)
  | Rematch
  | Chat Username Text
  deriving (Show)


initModel :: Turn -> Gen -> Model
initModel turn gen = Model turn [] handPA handPB deckPA deckPB maxLife maxLife NoPass [] gen
  where
    (genPA, genPB) = split gen :: (Gen, Gen)
    initDeckPA = shuffle initDeck genPA :: Deck
    (handPA, deckPA) = splitAt 4 initDeckPA :: (Hand, Deck)
    initDeckPB = shuffle initDeck genPB :: Deck
    (handPB, deckPB) = splitAt 4 initDeckPB :: (Hand, Deck)


initDeck :: Deck
initDeck =
  -- DAMAGE
     (replicate 3 cardFireball)
  ++ (replicate 3 cardDagger)
  ++ (replicate 3 cardBoomerang)
  ++ (replicate 3 cardPotion)
  ++ (replicate 3 cardVampire)
  ++ (replicate 3 cardSuccubus)
  -- ++ (replicate 3 cardObscurer)
  -- CONTROL
  ++ (replicate 2 cardSiren)
  ++ (replicate 2 cardSickness)
  -- HARD CONTROL
  ++ (replicate 2 cardHubris)
  ++ (replicate 2 cardReflect)
  ++ (replicate 2 cardReversal)
  ++ (replicate 2 cardConfound)
  -- SOFT CONTROL
  ++ (replicate 2 cardSiren)
  ++ (replicate 2 cardSickness)
  ++ (replicate 2 cardProphecy)
  -- ++ (replicate 2 cardOffering)
  -- ++ (replicate 2 cardGoatFlute)


reverso :: GameState -> GameState
reverso (Waiting gen)                   = Waiting gen
reverso (Started (Playing model))       = Started . Playing $ modelReverso model
reverso (Started (Ended which gen res)) = Started $ Ended (otherPlayer <$> which) gen (modelReverso <$> res)


update :: GameCommand -> WhichPlayer -> GameState -> Either Err (Maybe GameState, [Outcome])
update (Chat username msg) _ _ = Right (Nothing, [ChatOutcome username msg])
update cmd which state =
  case state of
    Waiting _ ->
      Left ("Unknown command " <> (cs $ show cmd) <> " on a waiting GameState")
    Started started ->
      case started of
        Playing model ->
          case cmd of
            EndTurn ->
              endTurn which (model { res = [] })
            PlayCard name ->
              ((\x -> (x, [])) . Just . Started . Playing) <$> (playCard name which (model { res = [] }))
            HoverCard name ->
              (\(_, y) -> (Nothing, y)) <$> (hoverCard name which (model { res = [] }))
            _ ->
              Left ("Unknown command " <> (cs $ show cmd) <> " on a Playing GameState")
        Ended winner gen _ ->
          case cmd of
            Rematch ->
                case winner of
                  Nothing ->
                    Right . (\x -> (x, [])) . Just . Started . Playing . (initModel PlayerA) . fst $ split gen
                  Just w ->
                    Right . (\x -> (x, [])) . Just . Started . Playing . (initModel w) . fst $ split gen
            _ ->
              Left ("Unknown command " <> (cs $ show cmd) <> " on an Ended GameState")


endTurn :: WhichPlayer -> Model -> Either Err (Maybe GameState, [Outcome])
endTurn which model@Model{..}
  | turn /= which = Left "You can't end the turn when it's not your turn!"
  | handFull = Right (Just . Started . Playing $ model, [])
  | otherwise =
    case passes of
      OnePass ->
        case resolveAll model of
          Playing m ->
            Right (Just . Started . Playing . drawCards . resetPasses . swapTurn $ m, [])
          Ended w g r ->
            Right (Just . Started $ Ended w g r, [])
      NoPass ->
        Right (Just . Started . Playing . swapTurn $ model, [])
  where
    handFull = length (getHand which model) >= maxHandLength :: Bool
    drawCards :: Model -> Model
    drawCards m = (drawCard PlayerA) . (drawCard PlayerB) $ m


resolveAll :: Model -> PlayState
resolveAll model@Model{ stack = stack } =
  if null stack then
    Playing model
    else
      case resolveOne model of
        Playing newModel ->
          resolveAll newModel
        s ->
          s
  where
    resolveOne :: Model -> PlayState
    resolveOne m =
      (rememberRes m) . lifeGate . eff $ modStack tailSafe model
      where
        eff :: Model -> Model
        eff = case headMay stack of
          Nothing -> id
          Just (StackCard p c@(Card _ _ _ e)) -> e p c

    rememberRes :: Model -> PlayState -> PlayState
    rememberRes r (Playing m@Model{ res = res }) =
      Playing (m { res = res ++ [r { res = [] }] })
    rememberRes r (Ended p gen res) =
      Ended p gen (res ++ [r { res = [] }])


lifeGate :: Model -> PlayState
lifeGate m@Model{..}
  | lifePA <= 0 && lifePB <= 0 =
    Ended Nothing gen res
  | lifePB <= 0 =
    Ended (Just PlayerA) gen res
  | lifePA <= 0 =
    Ended (Just PlayerB) gen res
  | otherwise =
    Playing $
      setLife PlayerA (min maxLife lifePA) $
        setLife PlayerB (min maxLife lifePB) m
  where
    lifePA = getLife PlayerA m :: Life
    lifePB = getLife PlayerB m :: Life
