module Login.Types exposing (..)


type alias Model =
    { username : String
    , password : String
    , error : String
    , submitting : Bool
    , nextUrl : String
    }


type alias LoginError =
    { error : String
    }
