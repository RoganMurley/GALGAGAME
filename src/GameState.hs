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
initModel = Model TurnPA [ cardDagger ] [ cardDagger ] (repeat cardHubris) (repeat cardHubris)


-- UPDATE

update :: GameCommand -> Model -> Model
update Draw model@(Model _ handPA handPB _ _) = drawCard model

drawCard :: Model -> Model
drawCard model@(Model turn handPA handPB deckPA deckPB)
  | (length hand < handMaxLength) = setDeck (tail deck) $ setHand (card : hand) model
  | otherwise = model
  where
    card :: Card
    card = head deck
    deck :: Deck
    deck = getDeck model
    hand :: Hand
    hand = getHand model

getHand :: Model -> Hand
getHand (Model TurnPA handPA handPB _ _) = handPA
getHand (Model TurnPB handPA handPB _ _) = handPB

setHand :: Hand -> Model -> Model
setHand newHand (Model TurnPA handPA handPB deckPA deckPB) = Model TurnPA newHand handPB deckPA deckPB
setHand newHand (Model TurnPB handPA handPB deckPA deckPB) = Model TurnPB handPA newHand deckPA deckPB

getDeck :: Model -> Deck
getDeck (Model TurnPA _ _ deckPA deckPB) = deckPA
getDeck (Model TurnPB _ _ deckPA deckPB) = deckPB

setDeck :: Deck -> Model -> Model
setDeck newDeck (Model TurnPA handPA handPB deckPA deckPB) = Model TurnPA handPA handPB newDeck deckPB
setDeck newDeck (Model TurnPB handPA handPB deckPA deckPB) = Model TurnPB handPA handPB deckPA newDeck


-- CARDS

cardDagger :: Card
cardDagger = Card "Dagger" "Hurt for 100" "plain-dagger.svg" "#bf1131"

cardHubris :: Card
cardHubris = Card "Hubris" "If combo is > 4 then negate everything" "tower-fall.svg" "#1c1f26"
