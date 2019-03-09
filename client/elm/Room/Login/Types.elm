module Login.Types exposing (Field(..), LoginError, Model)


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


type Field
    = UsernameField
    | PasswordField
