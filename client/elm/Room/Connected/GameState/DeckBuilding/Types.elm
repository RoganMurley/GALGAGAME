module DeckBuilding.Types exposing (Character, ChoosingCharacter(..), Model, RuneChoice)

import Buttons.Types exposing (Buttons)
import Dict exposing (Dict(..))
import RuneSelect.Types as RuneSelect exposing (Rune)
import Vfx.Types as Vfx


type alias Model =
    { character : Maybe Character
    , runes : List Rune
    , runeSelect : Maybe RuneSelect.Model
    , ready : Bool
    , bounceTick : Float
    , vfx : Vfx.Model
    , buttons : Buttons
    }


type alias Character =
    { choice : Maybe RuneChoice
    }


type ChoosingCharacter
    = ChosenCharacter Character
    | UnchosenCharacter (Maybe Character)


type alias RuneChoice =
    { runeA : Rune
    , runeB : Rune
    , runeC : Rune
    }
