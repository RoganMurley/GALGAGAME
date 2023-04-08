module Profile.Types exposing (Model, Profile, ProfileReplay)


type alias Model =
    { profile : Maybe Profile
    , error : String
    }


type alias Profile =
    { name : String
    , id : Int
    , xp : Float
    , level : Int
    , replays : List ProfileReplay
    , online : Bool
    , isMe : Bool
    }


type alias ProfileReplay =
    { id : Int
    , pa : Maybe String
    , pb : Maybe String
    }
