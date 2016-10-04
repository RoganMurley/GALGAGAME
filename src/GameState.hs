{-# LANGUAGE OverloadedStrings #-}
module GameState where

import Data.Text (Text)


type Card = Text
type Model = [Card]

data GameCommand = Draw


update :: GameCommand -> Model -> Model
update Draw model = drawCard model

drawCard :: Model -> Model
drawCard model = "blacklotus.jpg" : model
