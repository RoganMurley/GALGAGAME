module Model where


import Data.Aeson (ToJSON(..), (.=), object)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Safe (atMay, headMay, tailSafe)

import Mirror (Mirror(..))
import Player (WhichPlayer(..), other)
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

data Card = Card
  { card_name :: CardName
  , card_desc :: CardDesc
  , card_img  :: CardImgURL
  , card_snd  :: CardSndURL
  , card_eff  :: CardEff
  }

instance Eq Card where
  (Card n1 d1 i1 _ _) == (Card n2 d2 i2 _ _) =
    n1 == n2 && d1 == d2 && i1 == i2

instance Show Card where
  show = cs . card_name

type CardName = Text
type CardDesc = Text
type CardImgURL = Text
type CardSndURL = Text
type CardEff = (WhichPlayer -> Model -> Model)

type Hand = [Card]
type Deck = [Card]
type Stack = [StackCard]

data StackCard = StackCard
  { stackcard_owner :: WhichPlayer
  , stackcard_card  :: Card
  }
  deriving (Eq, Show)

type Life = Int

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
  toJSON StackCard{ stackcard_owner=owner, stackcard_card=card } =
    object [
      "owner" .= owner
    , "card"  .= card
    ]


maxHandLength :: Int
maxHandLength = 6


maxLife :: Life
maxLife = 50


instance Mirror Model where
  mirror (Model turn stack pa pb passes gen) =
    Model (mirror turn) (mirror <$> stack) pb pa passes gen


instance Mirror StackCard where
  mirror (StackCard p c) = StackCard (mirror p) c


swapTurn :: Model -> Model
swapTurn model = (modPasses incPasses) . (modTurn other) $ model


-- STACK CARD
changeOwner :: StackCard -> StackCard
changeOwner = mirror


withStackHead :: (StackCard -> CardEff) -> CardEff
withStackHead eff =
  (\p m ->
    case headMay (getStack m) of
      Nothing ->
        m
      Just card ->
        (eff card) p m
  )


-- GENERIC
type Setter   a b = b -> a -> a
type Getter   a b = a -> b
type Modifier a b = (b -> b) -> a -> a

modifier :: Setter a b -> Getter a b -> Modifier a b
modifier set get f = (\m -> set (f (get m)) m)


-- PLAYER MODEL
getPmodel :: WhichPlayer -> Getter Model PlayerModel
getPmodel PlayerA = model_pa
getPmodel PlayerB = model_pb

setPmodel :: PlayerModel -> WhichPlayer -> Model -> Model
setPmodel pmodel PlayerA model = model { model_pa = pmodel }
setPmodel pmodel PlayerB model = model { model_pb = pmodel }

modPmodel :: (PlayerModel -> PlayerModel) -> WhichPlayer -> Model -> Model
modPmodel f p m = setPmodel (f (getPmodel p m)) p m


-- TURN
getTurn :: Getter Model Turn
getTurn = model_turn

setTurn :: Setter Model Turn
setTurn turn model = model { model_turn = turn }

modTurn :: Modifier Model Turn
modTurn = modifier setTurn getTurn


-- LIFE.
getLife :: WhichPlayer -> Getter Model Life
getLife w m = pmodel_life $ getPmodel w m


setLife :: WhichPlayer -> Setter Model Life
setLife w life = modPmodel (\pm -> pm { pmodel_life = life }) w


modLife :: WhichPlayer -> Modifier Model Life
modLife w = modifier (setLife w) (getLife w)


-- HAND.
getHand :: WhichPlayer -> Getter Model Hand
getHand w m = pmodel_hand $ getPmodel w m


setHand :: WhichPlayer -> Setter Model Hand
setHand w h =
  modPmodel (\pm -> pm {
    -- Reverse once to make sure things are taken from the right end,
    -- then again to preserve initial order.
    pmodel_hand = reverse . (take maxHandLength) $ reverse h
  }) w


modHand :: WhichPlayer -> Modifier Model Hand
modHand w = modifier (setHand w) (getHand w)


-- DECK.
getDeck :: WhichPlayer -> Getter Model Deck
getDeck w m = pmodel_deck $ getPmodel w m


setDeck :: WhichPlayer -> Setter Model Deck
setDeck w d = modPmodel (\pm -> pm { pmodel_deck = d }) w


modDeck :: WhichPlayer -> Modifier Model Deck
modDeck w = modifier (setDeck w) (getDeck w)


-- STACK
getStack :: Getter Model Stack
getStack = model_stack


setStack :: Setter Model Stack
setStack stack model = model { model_stack = stack }


modStack :: Modifier Model Stack
modStack = modifier setStack getStack


modStackHead :: Modifier Model StackCard
modStackHead f m =
  case headMay stack of
    Nothing ->
      m
    Just c ->
      setStack (f c : (tailSafe stack)) m
  where
    stack = getStack m :: Stack


modStackAll :: Modifier Model StackCard
modStackAll f = modStack (fmap f)


-- Passes.
getPasses :: Getter Model Passes
getPasses = model_passes


setPasses :: Setter Model Passes
setPasses passes model = model { model_passes = passes }


modPasses :: Modifier Model Passes
modPasses = modifier setPasses getPasses


incPasses :: Passes -> Passes
incPasses NoPass = OnePass
incPasses OnePass = NoPass


resetPasses :: Model -> Model
resetPasses = setPasses NoPass


-- GEN
getGen :: Getter Model Gen
getGen = model_gen


-- ACTIONS
hurt :: Life -> WhichPlayer -> Model -> Model
hurt damage which model = modLife which (-damage+) model


heal :: Life -> WhichPlayer -> Model -> Model
heal life PlayerA model = modLife PlayerA (life+) model
heal life PlayerB model = modLife PlayerB (life+) model


lifesteal :: Life -> WhichPlayer -> Model -> Model
lifesteal damage which = (hurt damage which) . (heal damage (other which))


drawCard :: WhichPlayer -> Model -> Model
drawCard which model =
  case headMay (getDeck which model) of
    Just card ->
      modDeck which tailSafe $
        modHand which ((:) card) model
    Nothing ->
      modHand which ((:) theEnd) model
  where
    theEnd :: Card
    theEnd =
      Card
        "The End"
        "You're out of cards, hurt yourself for 10."
        "the_end.svg"
        "feint.wave"
        (hurt 10)


playCard :: Int -> WhichPlayer -> Model -> Either Err Model
playCard index which m
  | turn /= which = Left "You can't play a card when it's not your turn"
  | otherwise =
    case card of
      Nothing ->
        Left "You can't play a card you don't have in your hand"
      Just c ->
        Right
          . resetPasses
          . swapTurn
          . (modStack ((:) c))
          $ modHand which (deleteIndex index) m
  where
    hand = getHand which m :: Hand
    turn = getTurn m :: Turn
    card = (StackCard which) <$> (atMay hand index) :: Maybe StackCard


bounceAll :: WhichPlayer -> Model -> Model
bounceAll w m =
  (modStack (filter (not . owner)))
    . (modHand w ((++) (getCard <$> (filter owner s))))
      $ m
  where
    s :: Stack
    s = getStack m
    owner :: StackCard -> Bool
    owner (StackCard o _) = w == o
    getCard :: StackCard -> Card
    getCard (StackCard _ card) = card


both :: (WhichPlayer -> a -> a) -> a -> a
both f x = (f PlayerA) . (f PlayerB) $ x
