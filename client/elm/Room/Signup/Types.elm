module Signup.Types exposing (Field(..), Model, SignupError)

import Form exposing (FormField)


type alias Model =
    { email : FormField
    , username : FormField
    , password : FormField
    , error : String
    , submitting : Bool
    , nextUrl : String
    }


type alias SignupError =
    { error : String
    }


type Field
    = Email
    | Username
    | Password
