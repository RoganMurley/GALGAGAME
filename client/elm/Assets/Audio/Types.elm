module Audio.Types exposing (Model, PlayAudioMessage, SoundOption(..))

import Dict exposing (Dict)


type alias Model =
    { sounds : Dict String String }


type SoundOption
    = Once
    | Loop
    | Volume Float


type alias PlayAudioMessage =
    { name : String
    , once : Bool
    , loop : Bool
    , vol : Float
    }
