module Model where


import Data.Aeson (ToJSON(..), (.=), object)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Safe (atMay, headMay, tailSafe)

import Util (Err, Gen, deleteIndex)


data Model = Model
  { model_turn   :: Turn
  , model_stack  :: Stack
  , model_pa     :: PlayerModel
  , model_pb     :: PlayerModel
  , model_passes :: Passes
  , model_gen    :: Gen
  }
  deriving (Eq, Show)

data PlayerModel = PlayerModel
  { pmodel_hand :: Hand
  , pmodel_deck :: Deck
  , pmodel_life :: Life
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
modelReverso (Model turn stack pa pb passes gen) =
  (Model (other turn) (stackRev stack) pb pa passes gen)
  where
    stackRev :: Stack -> Stack
    stackRev = fmap (\(StackCard p c) -> StackCard (other p) c)


swapTurn :: Model -> Model
swapTurn model = (modPasses incPasses) . (modTurn other) $ model


other :: WhichPlayer -> WhichPlayer
other PlayerA = PlayerB
other PlayerB = PlayerA


-- PLAYER MODEL
getPmodel :: WhichPlayer -> Model -> PlayerModel
getPmodel PlayerA = model_pa
getPmodel PlayerB = model_pb

setPmodel :: PlayerModel -> WhichPlayer -> Model -> Model
setPmodel pmodel PlayerA model = model { model_pa = pmodel }
setPmodel pmodel PlayerB model = model { model_pb = pmodel }

modPmodel :: (PlayerModel -> PlayerModel) -> WhichPlayer -> Model -> Model
modPmodel f p m = setPmodel (f (getPmodel p m)) p m


-- TURN
getTurn :: Model -> Turn
getTurn = model_turn

setTurn :: Turn -> Model -> Model
setTurn turn model = model { model_turn = turn }

modTurn :: (Turn -> Turn) -> Model -> Model
modTurn f m = setTurn (f . getTurn $ m) m


-- LIFE.
getLife :: WhichPlayer -> Model -> Life
getLife w m = pmodel_life (getPmodel w m)


setLife :: Life -> WhichPlayer -> Model -> Model
setLife life = modPmodel (\pm -> pm { pmodel_life = life })


modLife :: (Life -> Life) -> WhichPlayer -> Model -> Model
modLife f p m = setLife (f (getLife p m)) p m


-- HAND.
getHand :: WhichPlayer -> Model -> Hand
getHand w m = pmodel_hand (getPmodel w m)


setHand :: Hand -> WhichPlayer -> Model -> Model
setHand hand = modPmodel (\pm -> pm { pmodel_hand = take maxHandLength hand })


modHand :: (Hand -> Hand) -> WhichPlayer -> Model -> Model
modHand f p m = setHand (f (getHand p m)) p m


-- DECK.
getDeck :: WhichPlayer -> Model -> Deck
getDeck w m = pmodel_deck (getPmodel w m)


setDeck :: Deck -> WhichPlayer -> Model -> Model
setDeck deck = modPmodel (\pm -> pm { pmodel_deck = deck })


modDeck :: (Deck -> Deck) -> WhichPlayer -> Model -> Model
modDeck f p m = setDeck (f (getDeck p m)) p m


-- STACK.
getStack :: Model -> Stack
getStack = model_stack


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
modStackAll f = modStack (fmap f)


-- Passes.
getPasses :: Model -> Passes
getPasses = model_passes


setPasses :: Passes -> Model -> Model
setPasses passes model = model { model_passes = passes }


modPasses :: (Passes -> Passes) -> Model -> Model
modPasses f m = setPasses (f . getPasses $ m) m


incPasses :: Passes -> Passes
incPasses NoPass = OnePass
incPasses OnePass = NoPass


resetPasses :: Model -> Model
resetPasses = setPasses NoPass


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
  heal d (other p) $ hurt d p m


drawCard :: WhichPlayer -> Model -> Model
drawCard which model =
  case headMay (getDeck which model) of
    Just card ->
      modDeck tailSafe which $ modHand ((:) card) which model
    Nothing ->
      model


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
          $ modHand (deleteIndex index) which m
      Nothing ->
        Left "You can't play a card you don't have in your hand"
  where
    hand = getHand which m :: Hand
    turn = getTurn m :: Turn
    card = (StackCard which) <$> (atMay hand index) :: Maybe StackCard


bounceAll :: WhichPlayer -> Stack -> Hand -> Hand
bounceAll w s h =
  h ++ (getCard <$> (filter owner s))
  where
    owner :: StackCard -> Bool
    owner (StackCard o _) = w == o
    getCard :: StackCard -> Card
    getCard (StackCard _ card) = card
