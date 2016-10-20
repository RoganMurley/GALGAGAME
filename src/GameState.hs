{-# LANGUAGE OverloadedStrings #-}
module GameState where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)


data Model = Model Turn Stack Hand Hand Deck Deck Life Life Passes
type Hand = [Card]
type Deck = [Card]
type Stack = [Card]
data Card = Card CardName CardDesc CardImgURL CardColor
type CardName = Text
type CardDesc = Text
type CardImgURL = Text
type CardColor = Text
type Life = Int

data WhichPlayer = PlayerA | PlayerB
  deriving (Eq)
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
  toJSON (Card name desc imageURL color) =
    object
      [
        "name" .= name
      , "desc" .= desc
      , "imageURL" .= imageURL
      , "cardColor" .= color
      ]

instance ToJSON WhichPlayer where
  toJSON PlayerA = "pa"
  toJSON PlayerB = "pb"

data GameCommand = Draw | EndTurn

handMaxLength :: Int
handMaxLength = 6

initModel :: Model
initModel = Model PlayerA [ cardFireball, cardHubris ] [ cardDagger ] [ cardHubris ] (cycle [cardHubris, cardFireball, cardDagger]) (cycle [cardHubris, cardDagger]) 1000 1000 NoPass


-- TEMP STUFF.
reverso :: Model -> Model
reverso (Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes) =
  Model (otherTurn turn) stack handPB handPA deckPB deckPA lifePA lifePB passes


-- UPDATE

update :: GameCommand -> WhichPlayer -> Model -> Maybe Model
update Draw which model = drawCard which model
update EndTurn which model = endTurn which model

drawCard :: WhichPlayer -> Model -> Maybe Model
drawCard which model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes)
  | (length hand >= handMaxLength) = Nothing
  | otherwise = Just (setDeck which (tail deck) $ setHand which (card : hand) model)
  where
    card :: Card
    card = head deck
    deck :: Deck
    deck = getDeck which model
    hand :: Hand
    hand = getHand which model

endTurn :: WhichPlayer -> Model -> Maybe Model
endTurn which model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes)
  | (turn == which) = drawCards (swapTurn passedModel)
  | otherwise = Nothing
  where
    bothPassed = (passes == OnePass) :: Bool
    passedModel :: Model
    passedModel = (Model turn stack handPA handPB deckPA deckPB lifePA lifePB (incPasses passes))
    drawCards :: Model -> Maybe Model
    drawCards m
      | bothPassed = (Just m) >>= (drawCard PlayerA) >>= (drawCard PlayerB)
      | otherwise = Just m

swapTurn :: Model -> Model
swapTurn model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB passes) =
  Model (otherTurn turn) stack handPA handPB deckPA deckPB lifePA lifePB passes

incPasses :: Passes -> Passes
incPasses NoPass = OnePass
incPasses OnePass = NoPass

otherTurn :: Turn -> Turn
otherTurn PlayerA = PlayerB
otherTurn PlayerB = PlayerA

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


-- CARDS

cardDagger :: Card
cardDagger = Card "Steeledge" "Hurt for 100" "plain-dagger.svg" "#bf1131"

cardHubris :: Card
cardHubris = Card "Hubris" "If combo is > 4 then negate everything" "tower-fall.svg" "#1c1f26"

cardFireball :: Card
cardFireball = Card "Meteor" "Hurt for 40 per combo" "fire-ray.svg" "#bf1131"
