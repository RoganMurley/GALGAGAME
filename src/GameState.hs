module GameState where

import Data.Aeson (ToJSON(..), (.=), object)
import Safe (headMay, tailSafe)
import System.Random (StdGen, split)

import Cards
import Model
import Util (shuffle)


data GameState =
    Waiting StdGen
  | Playing Model
  | Victory WhichPlayer StdGen ResolveList
  | Draw StdGen ResolveList


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


initModel :: Turn -> StdGen -> Model
initModel turn gen = Model turn [] handPA handPB deckPA deckPB maxLife maxLife Nothing Nothing NoPass [] gen
  where
    (genPA, genPB) = split gen :: (StdGen, StdGen)
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
          endTurn which (resetRes model)
        PlayCard name ->
          Playing (playCard name which (resetRes model))
        HoverCard name ->
          Playing (hoverCard name which (resetRes model))
        _ ->
          Playing model
    Victory winner gen res ->
      case cmd of
        Rematch ->
          Playing $ initModel winner (fst (split gen))
        _ ->
          Victory winner gen res
    Draw gen res ->
      case cmd of
        Rematch ->
          Playing $ initModel PlayerA (fst (split gen))
        _ ->
          Draw gen res
    s ->
      s

-- Make safer.
endTurn :: WhichPlayer -> Model -> GameState
endTurn which model
  | turn /= which = Playing model
  | handFull = Playing model
  | otherwise =
    case bothPassed of
      True ->
        case resolveAll model of
          Playing m ->
            Playing $ drawCards $ resetPasses $ swapTurn m
          s ->
            s
      False -> Playing $ swapTurn model
  where
    turn = getTurn model :: Turn
    passes = getPasses model :: Passes
    bothPassed = passes == OnePass :: Bool
    handFull = (length (getHand which model)) == maxHandLength :: Bool
    drawCards :: Model -> Model
    drawCards m = (drawCard PlayerA) . (drawCard PlayerB) $ m


resolveAll :: Model -> GameState
resolveAll model =
  case null (getStack model) of
    True -> Playing model
    False ->
      case resolveOne model of
        Playing newModel ->
          resolveAll newModel
        state ->
          state
  where
    resolveOne :: Model -> GameState
    resolveOne m =
      rememberRes m $ lifeGate $ eff $ modStack tailSafe model
      where
        stack = getStack model :: Stack
        eff :: Model -> Model
        eff = case headMay stack of
          Nothing -> id
          Just (StackCard p c@(Card _ _ _ effect)) -> effect p c

    rememberRes :: Model -> GameState -> GameState
    rememberRes r (Playing (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen)) =
      (Playing (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes (res ++ [resetRes r]) gen))
    rememberRes r (Victory p gen res) =
      Victory p gen (res ++ [resetRes r])
    rememberRes r (Draw gen res) =
      Draw gen (res ++ [resetRes r])
    rememberRes _ x = x


lifeGate :: Model -> GameState
lifeGate model
  | lifePA <= 0 && lifePB <= 0 =
    Draw gen res
  | lifePB <= 0 =
    Victory PlayerA gen res
  | lifePA <= 0 =
    Victory PlayerB gen res
  | otherwise =
    Playing $
      setLife PlayerA (min maxLife lifePA) $
        setLife PlayerB (min maxLife lifePB) model
  where
    gen = getGen model :: StdGen
    lifePA = getLife PlayerA model :: Life
    lifePB = getLife PlayerB model :: Life
    res = getRes model :: ResolveList
