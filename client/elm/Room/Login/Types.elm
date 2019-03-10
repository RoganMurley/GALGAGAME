module Login.Types exposing (Field(..), LoginError, Model)

import Form exposing (FormField)


type alias Model =
    { username : FormField
    , password : FormField
    , error : String
    , submitting : Bool
    , nextUrl : String
    }


type alias LoginError =
    { error : String
    }


type Field
    = Username
    | Password
