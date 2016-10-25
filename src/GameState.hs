{-# LANGUAGE OverloadedStrings #-}
module GameState where

import Control.Monad (MonadPlus, mplus)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.List (partition)
import Data.Maybe (isJust, maybeToList)
import Data.Text (Text)
import Safe (headMay, tailSafe)


data Model = Model Turn Stack Hand Hand Deck Deck Life Life Passes
type Hand = [Card]
type Deck = [Card]
type Stack = [StackCard]
data Card = Card CardName CardDesc CardImgURL CardColor CardEff
data StackCard = StackCard WhichPlayer Card
type CardName = Text
type CardDesc = Text
type CardImgURL = Text
type CardColor = Text
type Life = Int
type CardEff = (WhichPlayer -> Model -> Model)

data WhichPlayer = PlayerA | PlayerB
  deriving (Eq, Show)
type Turn = WhichPlayer

data Passes = NoPass | OnePass
  deriving (Eq)

instance ToJSON Model where
  toJSON (Model turn stack handPA handPB deckPA deckPB lifePA lifePB _) =
    object
      [
        "turn" .= turn
      , "stack" .= stack
      , "handPA" .= handPA
      , "handPB" .= handPB
      , "lifePA" .= lifePA
      , "lifePB" .= lifePB
      ]

instance ToJSON Card where
  toJSON (Card name desc imageURL color eff) =
    object
      [
        "name" .= name
      , "desc" .= desc
      , "imageURL" .= imageURL
      , "cardColor" .= color
      ]

instance ToJSON StackCard where
  toJSON (StackCard owner card) =
    toJSON card

instance ToJSON WhichPlayer where
  toJSON PlayerA = "pa"
  toJSON PlayerB = "pb"

data GameCommand =
    Draw
  | EndTurn
  | PlayCard CardName

handMaxLength :: Int
handMaxLength = 6

initModel :: Model
initModel = Model PlayerA [] hand hand deck deck 1000 1000 NoPass
  where
    (hand, deck) = splitAt 5 initDeck :: (Hand, Deck)

initDeck :: Deck
initDeck =
     (replicate 3 cardHubris)
  ++ (replicate 3 cardFireball)
  ++ (replicate 3 cardDagger)


-- TEMP STUFF.
reverso :: Model -> Model
reverso (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes) =
  Model (otherTurn turn) stack handPB handPA deckPB deckPA lifePB lifePA passes


-- UPDATE

update :: GameCommand -> WhichPlayer -> Model -> Maybe Model
update Draw which model = drawCard which model
update EndTurn which model = endTurn which model
update (PlayCard name) which model = playCard name which model

drawCard :: WhichPlayer -> Model -> Maybe Model
drawCard which model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes)
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

endTurn :: WhichPlayer -> Model -> Maybe Model
endTurn which model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes)
  | turn /= which = Nothing
  | otherwise =
    case bothPassed of
      True -> drawCards $ resetPasses $ swapTurn $ resolveAll $ model
      False -> Just $ swapTurn $ model
  where
    bothPassed :: Bool
    bothPassed = passes == OnePass
    drawCards :: Model -> Maybe Model
    drawCards m = (Just m) >>? (drawCard PlayerA) >>? (drawCard PlayerB)

(>>?) :: (MonadPlus m) => m a -> (a -> m a) -> m a
(>>?) x f = mplus (x >>= f) x

-- In future, tag cards in hand with a uid and use that.
playCard :: CardName -> WhichPlayer -> Model -> Maybe Model
playCard name which model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes)
  = card *> (Just (resetPasses $ swapTurn $ setStack ((maybeToList card) ++ stack) $ setHand which newHand model))
  where
    hand :: Hand
    hand = getHand which model
    (matches, misses) = partition (\(Card n _ _ _ _) -> n == name) hand :: ([Card], [Card])
    newHand :: Hand
    newHand = (tailSafe matches) ++ misses
    card :: Maybe StackCard
    card = (StackCard which) <$> (headMay matches)

swapTurn :: Model -> Model
swapTurn model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes) =
  Model (otherTurn turn) stack handPA handPB deckPA deckPB lifePA lifePB (incPasses passes)

incPasses :: Passes -> Passes
incPasses NoPass = OnePass
incPasses OnePass = NoPass

resetPasses :: Model -> Model
resetPasses (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes) =
  Model turn stack handPA handPB deckPA deckPB lifePA lifePB NoPass

otherTurn :: Turn -> Turn
otherTurn PlayerA = PlayerB
otherTurn PlayerB = PlayerA

otherPlayer :: WhichPlayer -> WhichPlayer
otherPlayer = otherTurn

getHand :: WhichPlayer -> Model -> Hand
getHand PlayerA (Model _ _ handPA handPB _ _ _ _ _) = handPA
getHand PlayerB (Model _ _ handPA handPB _ _ _ _ _) = handPB

setHand :: WhichPlayer -> Hand -> Model -> Model
setHand PlayerA newHand (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes) = Model turn stack newHand handPB deckPA deckPB lifePA lifePB passes
setHand PlayerB newHand (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes) = Model turn stack handPA newHand deckPA deckPB lifePA lifePB passes

getDeck :: WhichPlayer -> Model -> Deck
getDeck PlayerA (Model _ _ _ _ deckPA deckPB _ _ _) = deckPA
getDeck PlayerB (Model _ _ _ _ deckPA deckPB _ _ _) = deckPB

setDeck :: WhichPlayer -> Deck -> Model -> Model
setDeck PlayerA newDeck (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes) = Model turn stack handPA handPB newDeck deckPB lifePA lifePB passes
setDeck PlayerB newDeck (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes) = Model turn stack handPA handPB deckPA newDeck lifePA lifePB passes

getStack :: Model -> Stack
getStack (Model _ stack _ _ _ _ _ _ _) = stack

setStack :: Stack -> Model -> Model
setStack newStack (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes) =
  Model turn newStack handPA handPB deckPA deckPB lifePA lifePB passes

resolveOne :: Model -> Model
resolveOne model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes) =
  eff (setStack (tailSafe stack) model)
  where
    eff :: Model -> Model
    eff = case headMay stack of
      Nothing -> id
      Just (StackCard p (Card _ _ _ _ effect)) -> effect p

resolveAll :: Model -> Model
resolveAll model =
  case stackEmpty of
    True -> model
    False -> resolveAll (resolveOne model)
  where
    stackEmpty :: Bool
    stackEmpty = null (getStack model)

hurt :: Life -> WhichPlayer -> Model -> Model
hurt damage PlayerA (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes) =
  Model turn stack handPA handPB deckPA deckPB (lifePA - damage) lifePB passes
hurt damage PlayerB (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes) =
  Model turn stack handPA handPB deckPA deckPB lifePA (lifePB - damage) passes

-- CARDS

cardDagger :: Card
cardDagger = Card "Steeledge" "Hurt for 100" "plain-dagger.svg" "#bf1131" eff
  where
    eff :: CardEff
    eff p m = hurt 100 (otherPlayer p) m

cardHubris :: Card
cardHubris = Card "Hubris" "Negate whole combo" "tower-fall.svg" "#1c1f26" eff
  where
    eff :: CardEff
    eff p m = setStack [] m

cardFireball :: Card
cardFireball = Card "Meteor" "Hurt for 40 per combo" "fire-ray.svg" "#bf1131" eff
  where
    eff :: CardEff
    eff p m = hurt (40 * (length (getStack m))) (otherPlayer p) m
