{-# LANGUAGE OverloadedStrings #-}
module GameState where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)


data Model = Model Turn Hand Hand Deck Deck
type Hand = [Card]
type Deck = [Card]
data Card = Card CardName CardDesc CardImgURL CardColor
type CardName = Text
type CardDesc = Text
type CardImgURL = Text
type CardColor = Text

data WhichPlayer = PlayerA | PlayerB
  deriving (Eq)
type Turn = WhichPlayer

instance ToJSON Model where
  toJSON (Model turn handPA handPB deckPA deckPB) =
    object
      [
        "turn" .= turn
      , "handPA" .= handPA
      , "handPB" .= handPB
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
initModel = Model PlayerA [ cardDagger ] [ cardHubris ] (cycle [cardHubris, cardFireball, cardDagger]) (cycle [cardHubris, cardDagger])


-- TEMP STUFF.
reverso :: Model -> Model
reverso (Model turn handPA handPB deckPA deckPB) = Model (otherTurn turn) handPB handPA deckPB deckPA


-- UPDATE

update :: GameCommand -> WhichPlayer -> Model -> Maybe Model
update Draw which model = drawCard model which
update EndTurn which model = endTurn model which

drawCard :: Model -> WhichPlayer -> Maybe Model
drawCard model@(Model turn handPA handPB deckPA deckPB) which
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
endTurn model@(Model turn handPA handPB deckPA deckPB) which
  | (turn == which) = Just (Model (otherTurn turn) handPA handPB deckPA deckPB)
  | otherwise = Nothing

otherTurn :: Turn -> Turn
otherTurn PlayerA = PlayerB
otherTurn PlayerB = PlayerA

getHand :: WhichPlayer -> Model -> Hand
getHand PlayerA (Model _ handPA handPB _ _) = handPA
getHand PlayerB (Model _ handPA handPB _ _) = handPB

setHand :: WhichPlayer -> Hand -> Model -> Model
setHand PlayerA newHand (Model turn handPA handPB deckPA deckPB) = Model turn newHand handPB deckPA deckPB
setHand PlayerB newHand (Model turn handPA handPB deckPA deckPB) = Model turn handPA newHand deckPA deckPB

getDeck :: WhichPlayer -> Model -> Deck
getDeck PlayerA (Model _ _ _ deckPA deckPB) = deckPA
getDeck PlayerB (Model _ _ _ deckPA deckPB) = deckPB

setDeck :: WhichPlayer -> Deck -> Model -> Model
setDeck PlayerA newDeck (Model turn handPA handPB deckPA deckPB) = Model turn handPA handPB newDeck deckPB
setDeck PlayerB newDeck (Model turn handPA handPB deckPA deckPB) = Model turn handPA handPB deckPA newDeck


-- CARDS

cardDagger :: Card
cardDagger = Card "Steeledge" "Hurt for 100" "plain-dagger.svg" "#bf1131"

cardHubris :: Card
cardHubris = Card "Hubris" "If combo is > 4 then negate everything" "tower-fall.svg" "#1c1f26"

cardFireball :: Card
cardFireball = Card "Meteor" "Hurt for 40 per combo" "fire-ray.svg" "#bf1131"
