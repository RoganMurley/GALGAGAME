module Signup.Types exposing (Model, SignupError)


type alias Model =
    { email : String
    , username : String
    , password : String
    , confirmPassword : String
    , error : String
    , submitting : Bool
    , nextUrl : String
    }


type alias SignupError =
    { error : String
    }
