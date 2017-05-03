module Model where


import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import Safe (atMay, headMay, tailSafe)
import Data.String.Conversions (cs)

import Util (Err, Gen, deleteIndex)


data Model = Model
  { model_turn    :: Turn
  , model_stack   :: Stack
  , model_handPA  :: Hand
  , model_handPB  :: Hand
  , model_deckPA  :: Deck
  , model_deckPB  :: Deck
  , model_lifePA  :: Life
  , model_lifePB  :: Life
  , model_passes  :: Passes
  , model_gen     :: Gen
  }
  deriving (Eq, Show)


data Card = Card CardName CardDesc CardImgURL CardSndURL CardEff

instance Eq Card where
  (Card n1 d1 i1 _ _) == (Card n2 d2 i2 _ _) =
    n1 == n2 && d1 == d2 && i1 == i2

instance Show Card where
  show (Card n _ _ _ _) = cs n

type CardName = Text
type CardDesc = Text
type CardImgURL = Text
type CardSndURL = Text
type CardEff = (WhichPlayer -> Model -> Model)

type Hand = [Card]
type Deck = [Card]
type Stack = [StackCard]

data StackCard = StackCard WhichPlayer Card
  deriving (Eq, Show)

type Life = Int


data WhichPlayer = PlayerA | PlayerB
  deriving (Eq, Show)
type Turn = WhichPlayer

data Passes = NoPass | OnePass
  deriving (Eq, Show)


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
      ]


instance ToJSON Card where
  toJSON (Card name desc imageURL sfxURL _) =
    object
      [
        "name"     .= name
      , "desc"     .= desc
      , "imageURL" .= imageURL
      , "sfxURL"   .= sfxURL
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
modelReverso (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes gen) =
  (Model (otherTurn turn) (stackRev stack) handPB handPA deckPB deckPA lifePB lifePA passes gen)
  where
    stackRev :: Stack -> Stack
    stackRev s = fmap (\(StackCard p c) -> StackCard (otherPlayer p) c) s


swapTurn :: Model -> Model
swapTurn model = (modPasses incPasses) . (modTurn otherTurn) $ model


otherTurn :: Turn -> Turn
otherTurn PlayerA = PlayerB
otherTurn PlayerB = PlayerA


otherPlayer :: WhichPlayer -> WhichPlayer
otherPlayer = otherTurn


-- TURN
getTurn :: Model -> Turn
getTurn Model{ model_turn = turn } = turn

setTurn :: Turn -> Model -> Model
setTurn turn model = model { model_turn = turn }

modTurn :: (Turn -> Turn) -> Model -> Model
modTurn f m = setTurn (f . getTurn $ m) m


-- LIFE.
getLife :: WhichPlayer -> Model -> Life
getLife PlayerA = model_lifePA
getLife PlayerB = model_lifePB


setLife :: WhichPlayer -> Life -> Model -> Model
setLife PlayerA life model = model { model_lifePA = life }
setLife PlayerB life model = model { model_lifePB = life }


modLife :: (Life -> Life) -> WhichPlayer -> Model -> Model
modLife f p m = setLife p (f (getLife p m)) m


-- HAND.
getHand :: WhichPlayer -> Model -> Hand
getHand PlayerA = model_handPA
getHand PlayerB = model_handPB


setHand :: WhichPlayer -> Hand -> Model -> Model
setHand PlayerA hand model = model { model_handPA = take maxHandLength hand }
setHand PlayerB hand model = model { model_handPB = take maxHandLength hand }


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
getDeck PlayerA = model_deckPA
getDeck PlayerB = model_deckPB


setDeck :: WhichPlayer -> Deck -> Model -> Model
setDeck PlayerA deck model = model { model_deckPA = deck }
setDeck PlayerB deck model = model { model_deckPB = deck }


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
getStack Model{ model_stack = stack } = stack


setStack :: Stack -> Model -> Model
setStack stack model = model { model_stack = stack }


modStack :: (Stack -> Stack) -> Model -> Model
modStack f m = setStack (f . getStack $ m) m


modStackHead :: (StackCard -> StackCard) -> Model -> Model
modStackHead f m =
  case headMay stack of
    Nothing ->
      m
    Just c ->
      setStack (f c : (tailSafe stack)) m
  where
    stack = getStack m :: Stack


modStackAll :: (StackCard -> StackCard) -> Model -> Model
modStackAll f m = modStack (fmap f) m


-- Passes.
getPasses :: Model -> Passes
getPasses Model{ model_passes = passes } = passes


setPasses :: Passes -> Model -> Model
setPasses passes model = model { model_passes = passes }


modPasses :: (Passes -> Passes) -> Model -> Model
modPasses f m = setPasses (f . getPasses $ m) m


incPasses :: Passes -> Passes
incPasses NoPass = OnePass
incPasses OnePass = NoPass


resetPasses :: Model -> Model
resetPasses model = setPasses NoPass model


-- Gen
getGen :: Model -> Gen
getGen Model{ model_gen = gen } = gen


-- ACTIONS
hurt :: Life -> WhichPlayer -> Model -> Model
hurt damage which model =
  modLife (-damage+) which model


heal :: Life -> WhichPlayer -> Model -> Model
heal life PlayerA model =
  modLife (life+) PlayerA model
heal life PlayerB model =
  modLife (life+) PlayerB model


lifesteal :: Life -> WhichPlayer -> Model -> Model
lifesteal d p m =
  heal d (otherPlayer p) $ hurt d p m


drawCard :: WhichPlayer -> Model -> Model
drawCard which model =
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


playCard :: Int -> WhichPlayer -> Model -> Either Err Model
playCard index which m
  | turn /= which = Left "You can't play a card when it's not your turn"
  | otherwise =
    case card of
      Just c ->
        Right
          . resetPasses
          . swapTurn
          . (modStack ((:) c))
          $ setHand which newHand m
      Nothing ->
        Left "You can't play a card you don't have in your hand"
  where
    turn = getTurn m :: Turn
    hand = getHand which m :: Hand
    newHand = deleteIndex index hand :: Hand
    card = (StackCard which) <$> (atMay hand index) :: Maybe StackCard


bounceAll :: WhichPlayer -> Stack -> Hand -> Hand
bounceAll w s h =
  h ++ (getCard <$> (filter owner s))
  where
    owner :: StackCard -> Bool
    owner (StackCard o _) = w == o
    getCard :: StackCard -> Card
    getCard (StackCard _ card) = card
