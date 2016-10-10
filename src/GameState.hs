{-# LANGUAGE OverloadedStrings #-}
module GameState where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)


data Model = Model Turn Hand Hand
type Hand = [Card]
data Card = Card CardName CardDesc CardImgURL CardColor
type CardName = Text
type CardDesc = Text
type CardImgURL = Text
type CardColor = Text

instance ToJSON Model where
  toJSON (Model turn handPA handPB) =
    object ["handPA" .= handPA, "handPB" .= handPB]

instance ToJSON Card where
  toJSON (Card name desc imageURL color) =
    object ["name" .= name, "desc" .= desc, "imageURL" .= imageURL, "cardColor" .= color]

data Turn = TurnPA | TurnPB

data GameCommand = Draw

handMaxLength :: Int
handMaxLength = 6

initModel :: Model
initModel = Model TurnPA [ cardDagger ] [ cardDagger ]


-- UPDATE

update :: GameCommand -> Model -> Model
update Draw model@(Model _ handPA handPB) = drawCard cardHubris model

drawCard :: Card -> Model -> Model
drawCard card model@(Model turn handPA handPB)
  | (length hand < handMaxLength) = setHand (card : hand) model
  | otherwise = model
  where
    hand :: Hand
    hand = getHand model

getHand :: Model -> Hand
getHand (Model TurnPA handPA handPB) = handPA
getHand (Model TurnPB handPA handPB) = handPB

setHand :: Hand -> Model -> Model
setHand newHand (Model TurnPA handPA handPB) = Model TurnPA newHand handPB
setHand newHand (Model TurnPB handPA handPB) = Model TurnPB handPA newHand


-- CARDS

cardDagger :: Card
cardDagger = Card "Dagger" "Hurt for 100" "plain-dagger.svg" "#bf1131"

cardHubris :: Card
cardHubris = Card "Hubris" "If combo is > 4 then negate everything" "tower-fall.svg" "#1c1f26"
