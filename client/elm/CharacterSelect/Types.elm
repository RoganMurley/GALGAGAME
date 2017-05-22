module CharacterSelect.Types exposing (..)

import Card.Types exposing (Card)


type alias Name =
    String


type alias Character =
    { name : Name
    , imgURL : String
    , cards : ( Card, Card, Card, Card )
    }


type alias Model =
    { characters : List Character
    , selected : List Character
    , hover : Character
    }
