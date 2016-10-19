{-# LANGUAGE OverloadedStrings #-}
module GameState where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)


data Model = Model Turn Stack Hand Hand Deck Deck Life Life
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

instance ToJSON Model where
  toJSON (Model turn stack handPA handPB deckPA deckPB lifePA lifePB) =
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
initModel = Model PlayerA [ cardFireball, cardHubris ] [ cardDagger ] [ cardHubris ] (cycle [cardHubris, cardFireball, cardDagger]) (cycle [cardHubris, cardDagger]) 1000 1000


-- TEMP STUFF.
reverso :: Model -> Model
reverso (Model turn stack handPA handPB deckPA deckPB lifePA lifePB) =
  Model (otherTurn turn) stack handPB handPA deckPB deckPA lifePA lifePB


-- UPDATE

update :: GameCommand -> WhichPlayer -> Model -> Maybe Model
update Draw which model = drawCard model which
update EndTurn which model = endTurn model which

drawCard :: Model -> WhichPlayer -> Maybe Model
drawCard model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB) which
  | (turn /= which) = Nothing
  | (length hand >= handMaxLength) = Nothing
  | otherwise = Just (setDeck which (tail deck) $ setHand which (card : hand) model)
  where
    card :: Card
    card = head deck
    deck :: Deck
    deck = getDeck which model
    hand :: Hand
    hand = getHand which model

endTurn :: Model -> WhichPlayer -> Maybe Model
endTurn model@(Model turn stack handPA handPB deckPA deckPB lifePA lifePB) which
  | (turn == which) = Just (Model (otherTurn turn) stack handPA handPB deckPA deckPB lifePA lifePB)
  | otherwise = Nothing

otherTurn :: Turn -> Turn
otherTurn PlayerA = PlayerB
otherTurn PlayerB = PlayerA

getHand :: WhichPlayer -> Model -> Hand
getHand PlayerA (Model _ _ handPA handPB _ _ _ _) = handPA
getHand PlayerB (Model _ _ handPA handPB _ _ _ _) = handPB

setHand :: WhichPlayer -> Hand -> Model -> Model
setHand PlayerA newHand (Model turn stack handPA handPB deckPA deckPB lifePA lifePB) = Model turn stack newHand handPB deckPA deckPB lifePA lifePB
setHand PlayerB newHand (Model turn stack handPA handPB deckPA deckPB lifePA lifePB) = Model turn stack handPA newHand deckPA deckPB lifePA lifePB

getDeck :: WhichPlayer -> Model -> Deck
getDeck PlayerA (Model _ _ _ _ deckPA deckPB _ _) = deckPA
getDeck PlayerB (Model _ _ _ _ deckPA deckPB _ _) = deckPB

setDeck :: WhichPlayer -> Deck -> Model -> Model
setDeck PlayerA newDeck (Model turn stack handPA handPB deckPA deckPB lifePA lifePB) = Model turn stack handPA handPB newDeck deckPB lifePA lifePB
setDeck PlayerB newDeck (Model turn stack handPA handPB deckPA deckPB lifePA lifePB) = Model turn stack handPA handPB deckPA newDeck lifePA lifePB


-- CARDS

cardDagger :: Card
cardDagger = Card "Steeledge" "Hurt for 100" "plain-dagger.svg" "#bf1131"

cardHubris :: Card
cardHubris = Card "Hubris" "If combo is > 4 then negate everything" "tower-fall.svg" "#1c1f26"

cardFireball :: Card
cardFireball = Card "Meteor" "Hurt for 40 per combo" "fire-ray.svg" "#bf1131"
