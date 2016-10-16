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

instance ToJSON Model where
  toJSON (Model turn handPA handPB deckPA deckPB) =
    object ["handPA" .= handPA, "handPB" .= handPB]

instance ToJSON Card where
  toJSON (Card name desc imageURL color) =
    object ["name" .= name, "desc" .= desc, "imageURL" .= imageURL, "cardColor" .= color]

data Turn = TurnPA | TurnPB

data GameCommand = Draw

handMaxLength :: Int
handMaxLength = 6

initModel :: Model
initModel = Model TurnPA [ cardDagger ] [ cardHubris ] (cycle [cardHubris, cardFireball, cardDagger]) (cycle [cardHubris, cardDagger])


-- UPDATE

update :: GameCommand -> WhichPlayer -> Model -> Model
update Draw which model@(Model _ handPA handPB _ _) = drawCard model which

drawCard :: Model -> WhichPlayer -> Model
drawCard model@(Model turn handPA handPB deckPA deckPB) which
  | (length hand < handMaxLength) = setDeck which (tail deck) $ setHand which (card : hand) model
  | otherwise = model
  where
    card :: Card
    card = head deck
    deck :: Deck
    deck = getDeck which model
    hand :: Hand
    hand = getHand which model

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
cardDagger = Card "Blade's Edge" "Hurt for 100" "plain-dagger.svg" "#bf1131"

cardHubris :: Card
cardHubris = Card "Hubris" "If combo is > 4 then negate everything" "tower-fall.svg" "#1c1f26"

cardFireball :: Card
cardFireball = Card "Fireball" "Hurt for 40 per combo" "fire-ray.svg" "#bf1131"
