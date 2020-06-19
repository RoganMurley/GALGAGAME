module DeckBuilding.Types exposing (Character, Model)

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
    }


type alias Character =
    { name : String
    , imgUrl : String
    , runeA : Rune
    , runeB : Rune
    , runeC : Rune
    }
