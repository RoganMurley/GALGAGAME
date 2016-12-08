module Model where


import Data.Aeson (ToJSON(..), (.=), object)
import Data.List (findIndex, partition)
import Data.Text (Text)
import Safe (headMay, tailSafe)
import Data.String.Conversions (cs)

import Util (Gen)


data Model = Model
  { turn    :: Turn
  , stack   :: Stack
  , model_handPA  :: Hand
  , model_handPB  :: Hand
  , model_deckPA  :: Deck
  , model_deckPB  :: Deck
  , model_lifePA  :: Life
  , model_lifePB  :: Life
  , model_hoverPA :: HoverCardIndex
  , model_hoverPB :: HoverCardIndex
  , passes  :: Passes
  , res     :: ResolveList
  , gen     :: Gen
  }
  deriving (Eq, Show)


data Card = Card CardName CardDesc CardImgURL CardEff

instance Eq Card where
  (Card n1 d1 i1 _) == (Card n2 d2 i2 _) =
    n1 == n2 && d1 == d2 && i1 == i2

instance Show Card where
  show (Card n _ _ _) = cs n

type CardName = Text
type CardDesc = Text
type CardImgURL = Text
type CardEff = (WhichPlayer -> Card -> Model -> Model)

type Hand = [Card]
type Deck = [Card]
type Stack = [StackCard]

data StackCard = StackCard WhichPlayer Card
  deriving (Eq, Show)

type Life = Int
type ResolveList = [Model]
type HoverCardIndex = Maybe Int


data WhichPlayer = PlayerA | PlayerB
  deriving (Eq, Show)
type Turn = WhichPlayer

data Passes = NoPass | OnePass
  deriving (Eq, Show)


instance ToJSON Model where
  toJSON model@Model{..} =
    object
      [
        "turn"    .= turn
      , "stack"   .= stack
      , "handPA"  .= getHand PlayerA model
      , "handPB"  .= length (getHand PlayerB model)
      , "lifePA"  .= getLife PlayerA model
      , "lifePB"  .= getLife PlayerB model
      , "hoverPA" .= getHover PlayerA model
      , "hoverPB" .= getHover PlayerB model
      , "res"     .= res
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
swapTurn m@Model{ turn = turn } = m { turn = otherTurn turn }


otherTurn :: Turn -> Turn
otherTurn PlayerA = PlayerB
otherTurn PlayerB = PlayerA


otherPlayer :: WhichPlayer -> WhichPlayer
otherPlayer = otherTurn


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
setHand PlayerA hand model = model { model_handPA = hand }
setHand PlayerB hand model = model { model_handPB = hand }


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
mapStack :: (Stack -> a) -> Model -> a
mapStack f Model{ stack = stack } = f stack


modStack :: (Stack -> Stack) -> Model -> Model
modStack f m@Model{ stack = stack } = m { stack = f stack }


modStackHead :: (StackCard -> StackCard) -> Model -> Model
modStackHead f m@Model{ stack = stack } =
  case headMay stack of
    Nothing ->
      m
    Just c ->
      m { stack = f c : (tailSafe stack) }

modStackAll :: (StackCard -> StackCard) -> Model -> Model
modStackAll f m = modStack (fmap f) m


-- Passes.
incPasses :: Passes -> Passes
incPasses NoPass = OnePass
incPasses OnePass = NoPass

resetPasses :: Model -> Model
resetPasses model = model { passes = NoPass }


-- HOVER CARD.
getHover :: WhichPlayer -> Model -> HoverCardIndex
getHover PlayerA = model_hoverPA
getHover PlayerB = model_hoverPB


setHover :: WhichPlayer -> HoverCardIndex -> Model -> Model
setHover PlayerA hover model = model { model_hoverPA = hover }
setHover PlayerB hover model = model { model_hoverPB = hover }


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
playCard name which m@Model{..}
  | turn /= which = m
  | otherwise =
    case card of
      Just c ->
        resetPasses $ swapTurn $ modStack ((:) c) $ setHand which newHand m
      Nothing ->
        m
  where
    (matches, misses) = partition (\(Card n _ _ _) -> n == name) (getHand which m) :: ([Card], [Card])
    newHand = (tailSafe matches) ++ misses :: Hand
    card = (StackCard which) <$> (headMay matches) :: Maybe StackCard


hoverCard :: CardName -> WhichPlayer -> Model -> Model
hoverCard name which model = setHover which cardIndex model
  where
    cardIndex :: Maybe Int
    cardIndex = findIndex (\(Card n _ _ _) -> n == name) (getHand which model)


patchEff :: CardEff -> (Model -> Model -> Model) -> CardEff
patchEff eff wrapper = \w c m -> wrapper m (eff w c m)
