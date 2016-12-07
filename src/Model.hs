module Model where


import Data.Aeson (ToJSON(..), (.=), object)
import Data.List (findIndex, partition)
import Data.Text (Text)
import System.Random (StdGen, split)
import Safe (headMay, tailSafe)


data Model = Model Turn Stack Hand Hand Deck Deck Life Life HoverCardIndex HoverCardIndex Passes ResolveList StdGen

data Card = Card CardName CardDesc CardImgURL CardEff
type CardName = Text
type CardDesc = Text
type CardImgURL = Text
type CardEff = (WhichPlayer -> Card -> Model -> Model)

type Hand = [Card]
type Deck = [Card]
type Stack = [StackCard]

data StackCard = StackCard WhichPlayer Card

type Life = Int
type ResolveList = [Model]
type HoverCardIndex = Maybe Int


data WhichPlayer = PlayerA | PlayerB
  deriving (Eq, Show)
type Turn = WhichPlayer

data Passes = NoPass | OnePass
  deriving (Eq)


instance ToJSON Model where
  toJSON model =
    object
      [
        "turn"    .= getTurn model
      , "stack"   .= getStack model
      , "handPA"  .= getHand PlayerA model
      , "handPB"  .= length (getHand PlayerB model)
      , "lifePA"  .= getLife PlayerA model
      , "lifePB"  .= getLife PlayerB model
      , "hoverPA" .= getHover PlayerA model
      , "hoverPB" .= getHover PlayerB model
      , "res"     .= getRes model
      ]


instance ToJSON Card where
  toJSON (Card name desc imageURL eff) =
    object
      [
        "name"     .= name
      , "desc"     .= desc
      , "imageURL" .= imageURL
      ]


instance ToJSON StackCard where
  toJSON (StackCard owner card) =
    object [
      "owner" .= owner
    , "card"  .= card
    ]


instance ToJSON WhichPlayer where
  toJSON PlayerA = "pa"
  toJSON PlayerB = "pb"


maxHandLength :: Int
maxHandLength = 6


maxLife :: Life
maxLife = 50


modelReverso :: Model -> Model
modelReverso (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen) =
  (Model (otherTurn turn) (stackRev stack) handPB handPA deckPB deckPA lifePB lifePA hoverPB hoverPA passes (fmap modelReverso res) gen)
  where
    stackRev :: Stack -> Stack
    stackRev stack = fmap (\(StackCard p c) -> StackCard (otherPlayer p) c) stack


swapTurn :: Model -> Model
swapTurn model = modTurn otherTurn model


otherTurn :: Turn -> Turn
otherTurn PlayerA = PlayerB
otherTurn PlayerB = PlayerA


otherPlayer :: WhichPlayer -> WhichPlayer
otherPlayer = otherTurn


-- TURN.
getTurn :: Model -> Turn
getTurn (Model turn _ _ _ _ _ _ _ _ _ _ _ _) = turn


setTurn :: Turn -> Model -> Model
setTurn turn (Model _ stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen) =
  Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen


modTurn :: (Turn -> Turn) -> Model -> Model
modTurn f m = setTurn (f (getTurn m)) m


-- RNG GEN.
getGen :: Model -> StdGen
getGen (Model _ _ _ _ _ _ _ _ _ _ _ _ gen) = gen


-- LIFE.
getLife :: WhichPlayer -> Model -> Life
getLife PlayerA (Model _ _ _ _ _ _ lifePA _ _ _ _ _ _) = lifePA
getLife PlayerB (Model _ _ _ _ _ _ _ lifePB _ _ _ _ _) = lifePB


setLife :: WhichPlayer -> Life -> Model -> Model
setLife PlayerA lifePA (Model turn stack handPA handPB deckPA deckPB _ lifePB hoverPA hoverPB passes res gen) =
  Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen
setLife PlayerB lifePB (Model turn stack handPA handPB deckPA deckPB lifePA _ hoverPA hoverPB passes res gen) =
  Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen


modLife :: (Life -> Life) -> WhichPlayer -> Model -> Model
modLife f p m = setLife p (f (getLife p m)) m


-- HAND.
getHand :: WhichPlayer -> Model -> Hand
getHand PlayerA (Model _ _ handPA handPB _ _ _ _ _ _ _ _ _) = handPA
getHand PlayerB (Model _ _ handPA handPB _ _ _ _ _ _ _ _ _) = handPB


setHand :: WhichPlayer -> Hand -> Model -> Model
setHand PlayerA handPA (Model turn stack _ handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen) =
  Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen
setHand PlayerB handPB (Model turn stack handPA _ deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen) =
  Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen


mapHand :: (Hand -> a) -> WhichPlayer -> Model -> a
mapHand f p m = f (getHand p m)


modHand :: (Hand -> Hand) -> WhichPlayer -> Model -> Model
modHand f p m = setHand p (f (getHand p m)) m


addToHand :: Card -> Hand -> Hand
addToHand card hand
  | length hand < maxHandLength = card : hand
  | otherwise = hand


-- DECK.
getDeck :: WhichPlayer -> Model -> Deck
getDeck PlayerA (Model _ _ _ _ deckPA deckPB _ _ _ _ _ _ _) = deckPA
getDeck PlayerB (Model _ _ _ _ deckPA deckPB _ _ _ _ _ _ _) = deckPB


setDeck :: WhichPlayer -> Deck -> Model -> Model
setDeck PlayerA deckPA (Model turn stack handPA handPB _ deckPB lifePA lifePB hoverPA hoverPB passes res gen) =
  Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen
setDeck PlayerB deckPB (Model turn stack handPA handPB deckPA _ lifePA lifePB hoverPA hoverPB passes res gen) =
  Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen


mapDeck :: (Deck -> a) -> WhichPlayer -> Model -> a
mapDeck f p m = f (getDeck p m)


modDeck :: (Deck -> Deck) -> WhichPlayer -> Model -> Model
modDeck f p m = setDeck p (mapDeck f p m) m


modDeckHead :: (Card -> Card) -> WhichPlayer -> Model -> Model
modDeckHead f p m =
  case headMay (getDeck p m) of
    Just card ->
      setDeck p ((f card) : (tail (getDeck p m))) m
    Nothing ->
      m


-- STACK.
getStack :: Model -> Stack
getStack (Model _ stack _ _ _ _ _ _ _ _ _ _ _) = stack


setStack :: Stack -> Model -> Model
setStack stack (Model turn _ handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen) =
  Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen


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

modStackAll :: (StackCard -> StackCard) -> Model -> Model
modStackAll f m = modStack (fmap f) m


-- Passes.
getPasses :: Model -> Passes
getPasses (Model _ _ _ _ _ _ _ _ _ _ passes _ _) = passes

incPasses :: Passes -> Passes
incPasses NoPass = OnePass
incPasses OnePass = NoPass

resetPasses :: Model -> Model
resetPasses (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen) =
  Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB NoPass res gen


-- HOVER CARD.
getHover :: WhichPlayer -> Model -> HoverCardIndex
getHover PlayerA (Model _ _ _ _ _ _ _ _ hoverPA hoverPB _ _ _) = hoverPA
getHover PlayerB (Model _ _ _ _ _ _ _ _ hoverPA hoverPB _ _ _) = hoverPB


setHover :: WhichPlayer -> HoverCardIndex -> Model -> Model
setHover PlayerA hoverPA (Model turn stack handPA handPB deckPA deckPB lifePA lifePB _ hoverPB passes res gen) =
  (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen)
setHover PlayerB hoverPB (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA _ passes res gen) =
  (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen)


-- RESOLVING.
getRes :: Model -> ResolveList
getRes (Model _ _ _ _ _ _ _ _ _ _ _ res _) = res


resetRes :: Model -> Model
resetRes (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen) =
  (Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes [] gen)


-- ACTIONS
hurt :: Life -> WhichPlayer -> Model -> Model
hurt damage PlayerA model =
  modLife (\l -> l - damage) PlayerA model
hurt damage PlayerB model =
  modLife (\l -> l - damage) PlayerB model


heal :: Life -> WhichPlayer -> Model -> Model
heal life PlayerA model =
  modLife (\l -> l + life) PlayerA model
heal life PlayerB model =
  modLife (\l -> l + life) PlayerB model


lifesteal :: Life -> WhichPlayer -> Model -> Model
lifesteal d p m = heal d (otherPlayer p) $ hurt d p m


drawCard :: WhichPlayer -> Model -> Model
drawCard which model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB hoverPA hoverPB passes res gen)
  | (length hand >= maxHandLength) = model
  | otherwise =
    case drawnCard of
      Just card ->
        setDeck which drawnDeck $ setHand which (card : hand) model
      Nothing ->
        model
  where
    drawnCard :: Maybe Card
    drawnCard = headMay deck
    drawnDeck :: Deck
    drawnDeck = tailSafe deck
    deck :: Deck
    deck = getDeck which model
    hand :: Hand
    hand = getHand which model


-- In future, tag cards in hand with a uid and use that.
playCard :: CardName -> WhichPlayer -> Model -> Model
playCard name which model
  | turn /= which = model
  | otherwise =
    case card of
      Just c ->
        resetPasses $ swapTurn $ modStack ((:) c) $ setHand which newHand model
      Nothing ->
        model
  where
    turn = getTurn model :: Turn
    hand = getHand which model :: Hand
    (matches, misses) = partition (\(Card n _ _ _) -> n == name) hand :: ([Card], [Card])
    newHand = (tailSafe matches) ++ misses :: Hand
    card = (StackCard which) <$> (headMay matches) :: Maybe StackCard


hoverCard :: CardName -> WhichPlayer -> Model -> Model
hoverCard name which model = setHover which cardIndex model
  where
    cardIndex :: Maybe Int
    cardIndex = findIndex (\(Card n _ _ _) -> n == name) (getHand which model)
