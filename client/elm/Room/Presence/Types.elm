module Presence.Types exposing (Model, User)


type alias Model =
    { presence : Maybe (List User)
    , error : String
    }


type alias User =
    { id : Int
    , name : String
    }
