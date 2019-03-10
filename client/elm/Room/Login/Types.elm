module Login.Types exposing (Field(..), FormField, LoginError, Model, ValidationResult)

import Form exposing (Error(..))


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


type alias FormField =
    { value : String
    , touched : Bool
    }


type alias ValidationResult =
    { field : Field
    , error : Error
    , touched : Bool
    }


type Field
    = Username
    | Password
