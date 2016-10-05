{-# LANGUAGE OverloadedStrings #-}
module GameState where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)


type Card = Text
type Hand = [Card]
data Model = Model Hand Hand

instance ToJSON Model where
  toJSON (Model paHand pbHand) =
    object ["paHand" .= paHand, "pbHand" .= pbHand]

data GameCommand = Draw


update :: GameCommand -> Model -> Model
update Draw model = drawCard model

drawCard :: Model -> Model
drawCard (Model paHand pbHand) = Model ("blacklotus.jpg" : paHand) pbHand
