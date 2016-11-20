{-# LANGUAGE OverloadedStrings #-}
module GameState where

import Control.Applicative ((<$>))
import Control.Monad (MonadPlus, mplus)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.List (partition)
import Data.Maybe (isJust, maybeToList)
import Data.Text (Text)
import Safe (headMay, tailSafe)
import System.Random (StdGen, split)
import System.Random.Shuffle (shuffle')


data GameState = Waiting StdGen | Playing Model | Victory WhichPlayer StdGen | Draw StdGen

data Model = Model Turn Stack Hand Hand Deck Deck Life Life Passes StdGen
type Hand = [Card]
type Deck = [Card]
type Stack = [StackCard]
data Card = Card CardName CardDesc CardImgURL CardEff
data StackCard = StackCard WhichPlayer Card
type CardName = Text
type CardDesc = Text
type CardImgURL = Text
type Life = Int
type CardEff = (WhichPlayer -> Model -> Model)

data WhichPlayer = PlayerA | PlayerB
  deriving (Eq, Show)
type Turn = WhichPlayer

data Passes = NoPass | OnePass
  deriving (Eq)

instance ToJSON GameState where
  toJSON (Waiting _) =
    object [
      "waiting" .= True
    ]
  toJSON (Playing model) =
    toJSON model
  toJSON (Victory which gen) =
    object [
      "victory" .= which
    ]
  toJSON (Draw gen) =
    object [
      "draw" .= True
    ]

instance ToJSON Model where
  toJSON (Model turn stack handPA handPB deckPA deckPB lifePA lifePB _ _) =
    object
      [
        "playing" .=
          object
            [
              "turn" .= turn
            , "stack" .= stack
            , "handPA" .= handPA
            , "handPB" .= handPB
            , "lifePA" .= lifePA
            , "lifePB" .= lifePB
            ]
      ]

instance ToJSON Card where
  toJSON (Card name desc imageURL eff) =
    object
      [
        "name" .= name
      , "desc" .= desc
      , "imageURL" .= imageURL
      ]

instance ToJSON StackCard where
  toJSON (StackCard owner card) =
    object [
      "owner" .= owner
    , "card" .= card
    ]

instance ToJSON WhichPlayer where
  toJSON PlayerA = "pa"
  toJSON PlayerB = "pb"

data GameCommand =
    EndTurn
  | PlayCard CardName
  | Rematch

handMaxLength :: Int
handMaxLength = 6

lifeMax :: Life
lifeMax = 30

initModel :: Turn -> StdGen -> Model
initModel turn gen = Model turn [] handPA handPB deckPA deckPB lifeMax lifeMax NoPass gen
  where
    (genPA, genPB) = split gen :: (StdGen, StdGen)
    initDeckPA = shuffle' initDeck (length initDeck) genPA :: Deck
    (handPA, deckPA) = splitAt 5 initDeckPA :: (Hand, Deck)
    initDeckPB = shuffle' initDeck (length initDeck) genPB :: Deck
    (handPB, deckPB) = splitAt 5 initDeckPB :: (Hand, Deck)

initDeck :: Deck
initDeck =
  -- DAMAGE
     (replicate 3 cardFireball)
  ++ (replicate 3 cardDagger)
  ++ (replicate 3 cardBoomerang)
  ++ (replicate 3 cardPotion)
  ++ (replicate 3 cardVampire)
  ++ (replicate 3 cardSuccubus)
  ++ (replicate 3 cardGreed)
  ++ (replicate 3 cardSiren)
  -- CONTROL
  ++ (replicate 2 cardHubris)
  ++ (replicate 2 cardReflect)
  ++ (replicate 2 cardReversal)
  ++ (replicate 2 cardEcho)
  ++ (replicate 2 cardProphecy)


-- TEMP STUFF.
reverso :: GameState -> GameState
reverso (Playing (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen)) =
  (Playing (Model (otherTurn turn) (stackRev stack) handPB handPA deckPB deckPA lifePB lifePA passes gen))
  where
    stackRev :: Stack -> Stack
    stackRev stack = fmap (\(StackCard p c) -> StackCard (otherPlayer p) c) stack
reverso (Victory which gen) = Victory (otherPlayer which) gen
reverso x = x


-- UPDATE

update :: GameCommand -> WhichPlayer -> GameState -> Maybe GameState
update cmd which state =
  case state of
    (Playing model) ->
      case cmd of
        EndTurn ->
          endTurn which model
        PlayCard name ->
          Playing <$> (playCard name which model)
    (Victory winner gen) ->
      case cmd of
        Rematch ->
          Just $ Playing $ initModel winner (fst (split gen))
        x ->
          Just $ Victory winner gen
    (Draw gen) ->
      case cmd of
        Rematch ->
          Just $ Playing $ initModel PlayerA (fst (split gen))
        x ->
          Just $ Draw gen
    x ->
      Just x

drawCard :: WhichPlayer -> Model -> Maybe Model
drawCard which model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen)
  | (length hand >= handMaxLength) = Nothing
  | otherwise =
    case drawnCard of
      Just card ->
        Just (setDeck which drawnDeck $ setHand which (card : hand) model)
      Nothing ->
        Nothing
  where
    drawnCard :: Maybe Card
    drawnCard = headMay deck
    drawnDeck :: Deck
    drawnDeck = tailSafe deck
    deck :: Deck
    deck = getDeck which model
    hand :: Hand
    hand = getHand which model

-- Make safer.
endTurn :: WhichPlayer -> Model -> Maybe GameState
endTurn which model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen)
  | turn /= which = Nothing
  | otherwise =
    case bothPassed of
      True ->
        case resolveAll (Playing model) of
          Playing m ->
            Playing <$> (drawCards $ resetPasses $ swapTurn m)
          s ->
            Just s
      False -> Just $ Playing $ swapTurn $ model
  where
    bothPassed :: Bool
    bothPassed = passes == OnePass
    drawCards :: Model -> Maybe Model
    drawCards m = Just m >>? drawCard PlayerA >>? drawCard PlayerB

-- If a computation fails, ignore it.
(>>?) :: (MonadPlus m) => m a -> (a -> m a) -> m a
(>>?) x f = mplus (x >>= f) x

-- In future, tag cards in hand with a uid and use that.
playCard :: CardName -> WhichPlayer -> Model -> Maybe Model
playCard name which model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen)
  | turn /= which = Nothing
  | otherwise =
    card *> (Just (resetPasses $ swapTurn $ setStack ((maybeToList card) ++ stack) $ setHand which newHand model))
  where
    hand :: Hand
    hand = getHand which model
    (matches, misses) = partition (\(Card n _ _ _) -> n == name) hand :: ([Card], [Card])
    newHand :: Hand
    newHand = (tailSafe matches) ++ misses
    card :: Maybe StackCard
    card = (StackCard which) <$> (headMay matches)

swapTurn :: Model -> Model
swapTurn model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen) =
  Model (otherTurn turn) stack handPA handPB deckPA deckPB lifePA lifePB (incPasses passes) gen

incPasses :: Passes -> Passes
incPasses NoPass = OnePass
incPasses OnePass = NoPass

resetPasses :: Model -> Model
resetPasses (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen) =
  Model turn stack handPA handPB deckPA deckPB lifePA lifePB NoPass gen

otherTurn :: Turn -> Turn
otherTurn PlayerA = PlayerB
otherTurn PlayerB = PlayerA

otherPlayer :: WhichPlayer -> WhichPlayer
otherPlayer = otherTurn

-- HAND.
getHand :: WhichPlayer -> Model -> Hand
getHand PlayerA (Model _ _ handPA handPB _ _ _ _ _ _) = handPA
getHand PlayerB (Model _ _ handPA handPB _ _ _ _ _ _) = handPB

setHand :: WhichPlayer -> Hand -> Model -> Model
setHand PlayerA newHand (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen) =
  Model turn stack newHand handPB deckPA deckPB lifePA lifePB passes gen
setHand PlayerB newHand (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen) =
  Model turn stack handPA newHand deckPA deckPB lifePA lifePB passes gen

mapHand :: (Hand -> a) -> WhichPlayer -> Model -> a
mapHand f p m = f (getHand p m)

modHand :: (Hand -> Hand) -> WhichPlayer -> Model -> Model
modHand f p m = setHand p (f (getHand p m)) m

addToHand :: Card -> Hand -> Hand
addToHand card hand
  | length hand < handMaxLength =
    card : hand
  | otherwise = hand

-- DECK.
getDeck :: WhichPlayer -> Model -> Deck
getDeck PlayerA (Model _ _ _ _ deckPA deckPB _ _ _ _) = deckPA
getDeck PlayerB (Model _ _ _ _ deckPA deckPB _ _ _ _) = deckPB

setDeck :: WhichPlayer -> Deck -> Model -> Model
setDeck PlayerA newDeck (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen) =
  Model turn stack handPA handPB newDeck deckPB lifePA lifePB passes gen
setDeck PlayerB newDeck (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen) =
  Model turn stack handPA handPB deckPA newDeck lifePA lifePB passes gen

mapDeck :: (Deck -> a) -> WhichPlayer -> Model -> a
mapDeck f p m = f (getDeck p m)

modDeck :: (Deck -> Deck) -> WhichPlayer -> Model -> Model
modDeck f p m = setDeck p (mapDeck f p m) m

-- STACK.
getStack :: Model -> Stack
getStack (Model _ stack _ _ _ _ _ _ _ _) = stack

setStack :: Stack -> Model -> Model
setStack newStack (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen) =
  Model turn newStack handPA handPB deckPA deckPB lifePA lifePB passes gen

mapStack :: (Stack -> a) -> Model -> a
mapStack f m = f (getStack m)

modStack :: (Stack -> Stack) -> Model -> Model
modStack f m = setStack (mapStack f m) m

modStackHead :: (StackCard -> StackCard) -> Model -> Model
modStackHead f m =
  case headMay (getStack m) of
    Nothing ->
      m
    Just c ->
      (setStack (f c : (tailSafe (getStack m)))) m

resolveOne :: GameState -> GameState
resolveOne (Playing model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen)) =
  lifeGate $ Playing (eff (modStack tailSafe model))
  where
    eff :: Model -> Model
    eff = case headMay stack of
      Nothing -> id
      Just (StackCard p (Card _ _ _ effect)) -> effect p
    lifeGate :: GameState -> GameState
    lifeGate s@(Playing (Model _ _ _ _ _ _ lifePA lifePB _ _))
      | (lifePA <= 0) && (lifePB <= 0) = Draw gen
      | lifePB <= 0 = Victory PlayerA gen
      | lifePA <= 0 = Victory PlayerB gen
      | otherwise = s
    lifeGate x = x
resolveOne x = x

resolveAll :: GameState -> GameState
resolveAll state@(Playing model) =
  case stackEmpty of
    True -> state
    False -> resolveAll $ resolveOne state
  where
    stackEmpty :: Bool
    stackEmpty = null (getStack model)
resolveAll x = x

hurt :: Life -> WhichPlayer -> Model -> Model
hurt damage PlayerA (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen) =
  Model turn stack handPA handPB deckPA deckPB newLife lifePB passes gen
  where
    newLife
      | (lifePA - damage) < lifeMax = lifePA - damage
      | otherwise = lifeMax
hurt damage PlayerB (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen) =
  Model turn stack handPA handPB deckPA deckPB lifePA newLife passes gen
  where
    newLife
      | (lifePB - damage) < lifeMax = lifePB - damage
      | otherwise = lifeMax

-- CARDS

cardDagger :: Card
cardDagger = Card "Dagger" "Hurt for 5" "plain-dagger.svg" eff
  where
    eff :: CardEff
    eff p m = hurt 5 (otherPlayer p) m

cardHubris :: Card
cardHubris = Card "Hubris" "Negate all cards to the right" "tower-fall.svg" eff
  where
    eff :: CardEff
    eff p m = setStack [] m

cardFireball :: Card
cardFireball = Card "Fireball" "Hurt for 3 for each card to the right" "fire-ray.svg" eff
  where
    eff :: CardEff
    eff p m = hurt (3 * (length (getStack m))) (otherPlayer p) m

cardBoomerang :: Card
cardBoomerang = Card "Boomerang" "Hurt for 1, return this card to your hand" "boomerang.svg" eff
  where
    eff :: CardEff
    eff p m = setHand p (cardBoomerang : (getHand p m)) (hurt 1 (otherPlayer p) m)

cardPotion :: Card
cardPotion = Card "Potion" "Heal for 4" "heart-bottle.svg" eff
  where
    eff :: CardEff
    eff p m = hurt (-4) p m

cardVampire :: Card
cardVampire = Card "Vampire" "Lifesteal for 3" "fangs.svg" eff
  where
    eff :: CardEff
    eff p m = hurt 3 (otherPlayer p) $ hurt (-3) p m

cardSuccubus :: Card
cardSuccubus = Card "Succubus" "Lifesteal for 2 for each card to the right" "pretty-fangs.svg" eff
  where
    eff :: CardEff
    eff p m =
      hurt (2 * (length (getStack m))) (otherPlayer p) $
        hurt (-2 * ((length (getStack m)))) p m

cardReversal :: Card
cardReversal = Card "Reversal" "Reverse the order of cards to the right" "pocket-watch.svg" eff
  where
    eff :: CardEff
    eff p m = modStack reverse m

cardReflect :: Card
cardReflect = Card "Reflect" "Reflect the next card to the right" "shield-reflect.svg" eff
  where
    eff :: CardEff
    eff p m = modStackHead (reflectEff p) m
    reflectEff :: WhichPlayer -> StackCard -> StackCard
    reflectEff which (StackCard owner (Card name desc img cardEff)) =
      StackCard which (Card name desc img cardEff)

cardEcho :: Card
cardEcho = Card "Echo" "The next card to the right happens twice" "echo-ripples.svg" eff
  where
    eff :: CardEff
    eff p m = modStackHead echoEff m
    echoEff :: StackCard -> StackCard
    echoEff (StackCard owner (Card name desc img cardEff)) =
      StackCard owner (Card name desc img (echo cardEff))
    echo :: CardEff -> CardEff
    echo e = \which -> (e which) . (e which)

cardProphecy :: Card
cardProphecy = Card "Prophecy" "Return all cards to the right to their owner's hand" "crystal-ball.svg" eff
  where
    eff :: CardEff
    eff p m =
      modHand (bounceAll PlayerA (getStack m)) PlayerA $
      modHand (bounceAll PlayerB (getStack m)) PlayerB $
      setStack [] m
    bounceAll :: WhichPlayer -> Stack -> Hand -> Hand
    bounceAll w s h = (fmap getCard (filter (owner w) s)) ++ h
    owner :: WhichPlayer -> StackCard -> Bool
    owner PlayerA (StackCard PlayerA _) = True
    owner PlayerB (StackCard PlayerB _) = True
    owner _ _ = False
    getCard :: StackCard -> Card
    getCard (StackCard _ card) = card

cardGreed :: Card
cardGreed = Card "Greed" "Lifesteal for 1 for each card in your opponent's hand" "mouth-watering.svg" eff
  where
    eff :: CardEff
    eff p m =
      hurt (1 * length (getHand (otherPlayer p) m)) (otherPlayer p) $
        hurt (-1 * length (getHand (otherPlayer p) m)) p $
          m

cardSiren :: Card
cardSiren = Card "Siren" "Give your opponent two cards that hurt them for 3 damage" "harpy.svg" eff
  where
    eff :: CardEff
    eff p m = modHand ((addToHand cardSong) . (addToHand cardSong)) (otherPlayer p) m
    cardSong :: Card
    cardSong = Card "Siren's Song" "Hurt yourself for 3" "love-song.svg" (hurt 3)
