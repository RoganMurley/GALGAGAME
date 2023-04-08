module Entrypoint.Types exposing (Model, User)


type alias Model =
    { presence : Maybe (List User)
    , timer : Float
    , error : String
    }


type alias User =
    { id : Int
    , name : String
    }
