module Chat.Types exposing (Model)


type alias Model =
    { messages : List String
    , visible : Bool
    }
