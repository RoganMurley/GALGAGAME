{-# LANGUAGE OverloadedStrings #-}
module GameState where

import Control.Applicative ((<$>))
import Data.Aeson (ToJSON(..), (.=), object)
import Data.List (partition)
import Safe (headMay, tailSafe)
import System.Random (StdGen, split)
import System.Random.Shuffle (shuffle')

import Cards
import Model


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
  toJSON (Victory which gen res) =
    object [
      "victory" .= which
    , "res"     .= res
    ]
  toJSON (Draw gen res) =
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
    initDeckPA = shuffle' initDeck (length initDeck) genPA :: Deck
    (handPA, deckPA) = splitAt 4 initDeckPA :: (Hand, Deck)
    initDeckPB = shuffle' initDeck (length initDeck) genPB :: Deck
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
  -- ++ (replicate 2 cardEcho)
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
    (Playing model) ->
      case cmd of
        EndTurn ->
          endTurn which (resetRes model)
        PlayCard name ->
          Playing (playCard name which (resetRes model))
        HoverCard name ->
          Playing (hoverCard name which (resetRes model))
    (Victory winner gen res) ->
      case cmd of
        Rematch ->
          Playing $ initModel winner (fst (split gen))
        x ->
          Victory winner gen res
    (Draw gen res) ->
      case cmd of
        Rematch ->
          Playing $ initModel PlayerA (fst (split gen))
        x ->
          Draw gen res
    x ->
      x

-- Make safer.
endTurn :: WhichPlayer -> Model -> GameState
endTurn which model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen)
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
    bothPassed :: Bool
    bothPassed = passes == OnePass
    handFull ::  Bool
    handFull = (length (getHand which model)) == maxHandLength
    drawCards :: Model -> Model
    drawCards m = (drawCard PlayerA) . (drawCard PlayerB) $ m

-- In future, tag cards in hand with a uid and use that.
playCard :: CardName -> WhichPlayer -> Model -> Model
playCard name which model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen)
  | turn /= which = model
  | otherwise =
    case card of
      Just c ->
        resetPasses $ swapTurn $ modStack ((:) c) $ setHand which newHand model
      Nothing ->
        model
  where
    hand :: Hand
    hand = getHand which model
    (matches, misses) = partition (\(Card n _ _ _) -> n == name) hand :: ([Card], [Card])
    newHand :: Hand
    newHand = (tailSafe matches) ++ misses
    card :: Maybe StackCard
    card = (StackCard which) <$> (headMay matches)


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
    resolveOne model =
      rememberRes model $ lifeGate $ eff $ modStack tailSafe model
      where
        stack = getStack model :: Stack
        gen = getGen model :: StdGen
        eff :: Model -> Model
        eff = case headMay stack of
          Nothing -> id
          Just (StackCard p (Card _ _ _ effect)) -> effect p
        lifeGate :: Model -> GameState
        lifeGate m@(Model _ _ _ _ _ _ lifePA lifePB _ _ _ res _)
          | (lifePA <= 0) && (lifePB <= 0) = Draw gen res
          | lifePB <= 0 = Victory PlayerA gen res
          | lifePA <= 0 = Victory PlayerB gen res
          | otherwise = Playing (maxLifeGate m)
        maxLifeGate :: Model -> Model
        maxLifeGate m =
          setLife PlayerA (min maxLife (getLife PlayerA m)) $
            setLife PlayerB (min maxLife (getLife PlayerB m)) m

    rememberRes :: Model -> GameState -> GameState
    rememberRes r (Playing m@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen)) =
      (Playing (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes (res ++ [resetRes r]) gen))
    rememberRes r (Victory p gen res) =
      Victory p gen (res ++ [resetRes r])
    rememberRes r (Draw gen res) =
      Draw gen (res ++ [resetRes r])
    rememberRes r x = x
