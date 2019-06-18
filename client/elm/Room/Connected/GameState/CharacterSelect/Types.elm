module CharacterSelect.Types exposing (Character, CharacterCards(..), Model, Slot(..), ViewModel)

import Card.Types exposing (Card)


type alias Character =
    { name : String
    , imgURL : String
    , cards : CharacterCards
    }


type CharacterCards
    = CharacterCards Card Card Card Card


type alias Model =
    { characters : List Character
    , selected : List Character
    , vm : ViewModel
    }


type alias ViewModel =
    { hover : Character }


type Slot
    = SlotA
    | SlotB
    | SlotC
