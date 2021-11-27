module Chat.Types exposing (Model)


type alias Model =
    { input : String
    , messages : List String
    , visible : Bool
    }
