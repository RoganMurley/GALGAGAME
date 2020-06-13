module DeckBuilding.Types exposing (Character, Model)

import Carousel exposing (Carousel)
import RuneSelect.Types as RuneSelect exposing (Rune)


type alias Model =
    { characters : Carousel Character
    , runes : List Rune
    , runeSelect : Maybe RuneSelect.Model
    , ready : Bool
    , bounceTick : Float
    }


type alias Character =
    { name : String
    , imgUrl : String
    , runeA : Rune
    , runeB : Rune
    , runeC : Rune
    }
