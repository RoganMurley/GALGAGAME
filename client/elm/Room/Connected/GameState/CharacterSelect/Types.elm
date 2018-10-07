module CharacterSelect.Types exposing (..)

import Card.Types exposing (Card)


type alias Character =
    { name : String
    , imgURL : String
    , cards : CharacterCards
    }


type alias CharacterCards =
    ( Card, Card, Card, Card )


type alias Model =
    { characters : List Character
    , selected : List Character
    , vm : ViewModel
    }


type alias ViewModel =
    { hover : Character }
