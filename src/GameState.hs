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

initModel :: StdGen -> Model
initModel gen = Model PlayerA [] handPA handPB deckPA deckPB lifeMax lifeMax NoPass gen
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
  -- CONTROL
  ++ (replicate 2 cardHubris)
  ++ (replicate 2 cardReflect)
  ++ (replicate 2 cardReversal)


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
          Just $ Playing $ initModel gen
        x ->
          Just $ Victory winner gen
    (Draw gen) ->
      case cmd of
        Rematch ->
          Just $ Playing $ initModel gen
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
cardHubris = Card "Hubris" "Negate the whole stack" "tower-fall.svg" eff
  where
    eff :: CardEff
    eff p m = setStack [] m

cardFireball :: Card
cardFireball = Card "Fireball" "Hurt for 2 per stack" "fire-ray.svg" eff
  where
    eff :: CardEff
    eff p m = hurt (2 * ((length (getStack m)) + 1)) (otherPlayer p) m

cardBoomerang :: Card
cardBoomerang = Card "Boomerang" "Hurt for 1, get back at bottom of stack" "boomerang.svg" eff
  where
    eff :: CardEff
    eff p m = setStack ((getStack m) ++ [StackCard p cardCatch]) (hurt 1 (otherPlayer p) m)
    cardCatch :: Card
    cardCatch = Card "Catch Boomerang" "Get boomerang back" "hand.svg" catchEff
    catchEff :: CardEff
    catchEff p m = setHand p (cardBoomerang : (getHand p m)) m

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
cardSuccubus = Card "Succubus" "Lifesteal for 2 per stack card" "pretty-fangs.svg" eff
  where
    eff :: CardEff
    eff p m =
      hurt (2 * ((length (getStack m)) + 1)) (otherPlayer p) $
        hurt (-2 * ((length (getStack m)) + 1)) p m

cardReversal :: Card
cardReversal = Card "Reversal" "Reverse the order of the stack" "pocket-watch.svg" eff
  where
    eff :: CardEff
    eff p m = modStack reverse m

cardReflect :: Card
cardReflect = Card "Reflect" "Reflect the next card in the stack" "shield-reflect.svg" eff
  where
    eff :: CardEff
    eff p m =
      case headMay (getStack m) of
        Nothing ->
          m
        Just (StackCard owner card) ->
          (setStack ( (StackCard (otherPlayer owner) card) : (tailSafe (getStack m)))) m
