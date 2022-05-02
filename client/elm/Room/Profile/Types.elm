module Profile.Types exposing (Model, Profile)


type alias Model =
    { profile : Maybe Profile
    , error : String
    }


type alias Profile =
    { name : String
    , xp : Float
    , level : Int
    }
