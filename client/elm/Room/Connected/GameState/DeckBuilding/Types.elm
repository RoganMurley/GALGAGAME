module DeckBuilding.Types exposing (Button, Character, Model)

import Carousel exposing (Carousel)
import RuneSelect.Types as RuneSelect exposing (Rune)
import Vfx.Types as Vfx


type alias Model =
    { characters : Carousel Character
    , runes : List Rune
    , runeSelect : Maybe RuneSelect.Model
    , ready : Bool
    , bounceTick : Float
    , vfx : Vfx.Model
    , buttons : Buttons
    }


type alias Character =
    { name : String
    , imgUrl : String
    , runeA : Rune
    , runeB : Rune
    , runeC : Rune
    }


type alias Button =
    { x : Float
    , y : Float
    , size : Float
    , hover : Float
    }


type alias Buttons =
    { ready : Maybe Button
    }
