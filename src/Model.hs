{-# LANGUAGE RecordWildCards #-}
module Model where


import Data.Aeson (ToJSON(..), (.=), object)
import Data.List (findIndex, partition)
import Data.Text (Text)
import System.Random (StdGen)
import Safe (headMay, tailSafe)


data Model = Model
  { _turn    :: Turn
  , _stack   :: Stack
  , _handPA  :: Hand
  , _handPB  :: Hand
  , _deckPA  :: Deck
  , _deckPB  :: Deck
  , _lifePA  :: Life
  , _lifePB  :: Life
  , _hoverPA :: HoverCardIndex
  , _hoverPB :: HoverCardIndex
  , _passes  :: Passes
  , _res     :: ResolveList
  , _gen     :: StdGen
  }

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
  toJSON (Card name desc imageURL _) =
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
    stackRev s = fmap (\(StackCard p c) -> StackCard (otherPlayer p) c) s


swapTurn :: Model -> Model
swapTurn model = modTurn otherTurn model


otherTurn :: Turn -> Turn
otherTurn PlayerA = PlayerB
otherTurn PlayerB = PlayerA


otherPlayer :: WhichPlayer -> WhichPlayer
otherPlayer = otherTurn


-- TURN.
getTurn :: Model -> Turn
getTurn = _turn


setTurn :: Turn -> Model -> Model
setTurn turn model = model { _turn = turn }


modTurn :: (Turn -> Turn) -> Model -> Model
modTurn f m = setTurn (f (getTurn m)) m


-- RNG GEN.
getGen :: Model -> StdGen
getGen = _gen


-- LIFE.
getLife :: WhichPlayer -> Model -> Life
getLife PlayerA = _lifePA
getLife PlayerB = _lifePB


setLife :: WhichPlayer -> Life -> Model -> Model
setLife PlayerA life model = model { _lifePA = life }
setLife PlayerB life model = model { _lifePB = life }


modLife :: (Life -> Life) -> WhichPlayer -> Model -> Model
modLife f p m = setLife p (f (getLife p m)) m


-- HAND.
getHand :: WhichPlayer -> Model -> Hand
getHand PlayerA = _handPA
getHand PlayerB = _handPB


setHand :: WhichPlayer -> Hand -> Model -> Model
setHand PlayerA hand model = model { _handPA = hand }
setHand PlayerB hand model = model { _handPB = hand }


modHand :: (Hand -> Hand) -> WhichPlayer -> Model -> Model
modHand f p m = setHand p (f (getHand p m)) m


mapHand :: (Hand -> a) -> WhichPlayer -> Model -> a
mapHand f p m = f (getHand p m)


addToHand :: Card -> Hand -> Hand
addToHand card hand
  | length hand < maxHandLength = card : hand
  | otherwise = hand


-- DECK.
getDeck :: WhichPlayer -> Model -> Deck
getDeck PlayerA = _deckPA
getDeck PlayerB = _deckPB


setDeck :: WhichPlayer -> Deck -> Model -> Model
setDeck PlayerA deck model = model { _deckPA = deck }
setDeck PlayerB deck model = model { _deckPB = deck }


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
getStack = _stack


setStack :: Stack -> Model -> Model
setStack stack model = model { _stack = stack }


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
getPasses = _passes

incPasses :: Passes -> Passes
incPasses NoPass = OnePass
incPasses OnePass = NoPass

resetPasses :: Model -> Model
resetPasses model = model { _passes = NoPass }


-- HOVER CARD.
getHover :: WhichPlayer -> Model -> HoverCardIndex
getHover PlayerA = _hoverPA
getHover PlayerB = _hoverPB


setHover :: WhichPlayer -> HoverCardIndex -> Model -> Model
setHover PlayerA hover model = model { _hoverPA = hover }
setHover PlayerB hover model = model { _hoverPB = hover }


-- RESOLVING.
getRes :: Model -> ResolveList
getRes = _res


resetRes :: Model -> Model
resetRes model = model { _res = [] }


-- ACTIONS
hurt :: Life -> WhichPlayer -> Model -> Model
hurt damage which model = modLife (\l -> l - damage) which model


heal :: Life -> WhichPlayer -> Model -> Model
heal life PlayerA model =
  modLife (\l -> l + life) PlayerA model
heal life PlayerB model =
  modLife (\l -> l + life) PlayerB model


lifesteal :: Life -> WhichPlayer -> Model -> Model
lifesteal d p m = heal d (otherPlayer p) $ hurt d p m


drawCard :: WhichPlayer -> Model -> Model
drawCard which model
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


patchEff :: CardEff -> (Model -> Model -> Model) -> CardEff
patchEff eff wrapper = \w c m -> wrapper m (eff w c m)
