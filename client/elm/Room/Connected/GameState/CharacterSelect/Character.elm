module CharacterSelect.Character exposing (..)

import Card.Types exposing (Card)


type alias Character =
    { name : Name
    , imgURL : String
    , cards : ( Card, Card, Card, Card )
    }


type alias Name =
    String
