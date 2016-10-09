{-# LANGUAGE OverloadedStrings #-}
module GameState where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)


data Model = Model Hand Hand
type Hand = [Card]
data Card = Card CardName CardDesc CardImgURL CardColor
type CardName = Text
type CardDesc = Text
type CardImgURL = Text
type CardColor = Text

instance ToJSON Model where
  toJSON (Model paHand pbHand) =
    object ["paHand" .= paHand, "pbHand" .= pbHand]

instance ToJSON Card where
  toJSON (Card name desc imageURL color) =
    object ["name" .= name, "desc" .= desc, "imageURL" .= imageURL, "cardColor" .= color]

data GameCommand = Draw

handMaxLength :: Int
handMaxLength = 6


update :: GameCommand -> Model -> Model
update Draw model = drawCard cardHubris model

drawCard :: Card -> Model -> Model
drawCard card model@(Model paHand pbHand)
  | (length paHand < handMaxLength) = Model (card : paHand) pbHand
  | otherwise = model

-- Cards
cardDagger :: Card
cardDagger = Card "Dagger" "Hurt for 100" "plain-dagger.svg" "#bf1131"

cardHubris :: Card
cardHubris = Card "Hubris" "If combo is > 4 then negate everything" "tower-fall.svg" "#1c1f26"
