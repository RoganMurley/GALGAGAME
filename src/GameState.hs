module GameState where

import Data.Aeson (ToJSON(..), (.=), object)
import Safe (headMay, tailSafe)

import Cards
import Model
import Util (Gen, shuffle, split)


data GameState =
    Waiting Gen
  | Playing Model
  | Victory WhichPlayer Gen ResolveList
  | Draw Gen ResolveList
  deriving (Eq, Show)


instance ToJSON GameState where
  toJSON (Waiting _) =
    object [
      "waiting" .= True
    ]
  toJSON (Playing model) =
    object [
      "playing" .= toJSON model
    ]
  toJSON (Victory which _ res) =
    object [
      "victory" .= which
    , "res"     .= res
    ]
  toJSON (Draw _ res) =
    object [
      "draw" .= True
    , "res"  .= res
    ]


data GameCommand =
    EndTurn
  | PlayCard CardName
  | HoverCard CardName
  | Rematch


initModel :: Turn -> Gen -> Model
initModel turn gen = Model turn [] handPA handPB deckPA deckPB maxLife maxLife Nothing Nothing NoPass [] gen
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
  -- CONTROL
  ++ (replicate 2 cardSiren)
  ++ (replicate 2 cardSickness)
  -- ++ (replicate 3 cardObscurer)
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
reverso (Playing model) = Playing (modelReverso model)
reverso (Victory which gen res) = Victory (otherPlayer which) gen (fmap modelReverso res)
reverso (Draw gen res) = Draw gen (fmap modelReverso res)
reverso (Waiting gen) = Waiting gen


update :: GameCommand -> WhichPlayer -> GameState -> GameState
update cmd which state =
  case state of
    Playing model ->
      case cmd of
        EndTurn ->
          endTurn which (model { res = [] })
        PlayCard name ->
          Playing (playCard name which (model { res = [] }))
        HoverCard name ->
          Playing (hoverCard name which (model { res = [] }))
        _ ->
          Playing (model { res = [] })
    Victory winner gen res ->
      case cmd of
        Rematch ->
          Playing . (initModel winner) . fst $ split gen
        _ ->
          Victory winner gen res
    Draw gen res ->
      case cmd of
        Rematch ->
          Playing . (initModel PlayerA) . fst $ split gen
        _ ->
          Draw gen res
    s ->
      s

-- Make safer.
endTurn :: WhichPlayer -> Model -> GameState
endTurn which model@Model{..}
  | turn /= which = Playing model
  | handFull = Playing model
  | otherwise =
    case passes of
      OnePass ->
        case resolveAll model of
          Playing m ->
            Playing . drawCards . resetPasses . swapTurn $ m
          s ->
            s
      _ -> Playing . swapTurn $ model
  where
    handFull = length (getHand which model) >= maxHandLength :: Bool
    drawCards :: Model -> Model
    drawCards m = (drawCard PlayerA) . (drawCard PlayerB) $ m


resolveAll :: Model -> GameState
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
    resolveOne :: Model -> GameState
    resolveOne m =
      (rememberRes m) . lifeGate . eff $ modStack tailSafe model
      where
        eff :: Model -> Model
        eff = case headMay stack of
          Nothing -> id
          Just (StackCard p c@(Card _ _ _ e)) -> e p c

    rememberRes :: Model -> GameState -> GameState
    rememberRes r (Playing m@Model{ res = res }) =
      Playing (m { res = res ++ [r { res = [] }] })
    rememberRes r (Victory p gen res) =
      Victory p gen (res ++ [r { res = [] }])
    rememberRes r (Draw gen res) =
      Draw gen (res ++ [r { res = [] }])
    rememberRes _ s = s


lifeGate :: Model -> GameState
lifeGate m@Model{..}
  | lifePA <= 0 && lifePB <= 0 =
    Draw gen res
  | lifePB <= 0 =
    Victory PlayerA gen res
  | lifePA <= 0 =
    Victory PlayerB gen res
  | otherwise =
    Playing $
      setLife PlayerA (min maxLife lifePA) $
        setLife PlayerB (min maxLife lifePB) m
  where
    lifePA = getLife PlayerA m :: Life
    lifePB = getLife PlayerB m :: Life
