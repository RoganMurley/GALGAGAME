module Presence.Types exposing (Model)


type alias Model =
    { presence : Maybe (List String)
    , error : String
    }
