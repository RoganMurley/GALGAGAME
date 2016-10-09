{-# LANGUAGE OverloadedStrings #-}
module GameState where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)


data Model = Model Hand Hand
type Hand = [Card]
data Card = Card CardName CardDesc CardImgURL
type CardName = Text
type CardDesc = Text
type CardImgURL = Text

instance ToJSON Model where
  toJSON (Model paHand pbHand) =
    object ["paHand" .= paHand, "pbHand" .= pbHand]

instance ToJSON Card where
  toJSON (Card name desc imageURL) =
    object ["name" .= name, "desc" .= desc, "imageURL" .= imageURL]

data GameCommand = Draw

handMaxLength :: Int
handMaxLength = 6


update :: GameCommand -> Model -> Model
update Draw model = drawCard cardDagger model

drawCard :: Card -> Model -> Model
drawCard card model@(Model paHand pbHand)
  | (length paHand < handMaxLength) = Model (card : paHand) pbHand
  | otherwise = model

-- Cards
cardDagger :: Card
cardDagger = Card "Dagger" "Hurts a little" "plain-dagger.svg"
