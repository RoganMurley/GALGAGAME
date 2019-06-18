module Audio.Types exposing (PlayAudioMessage, SoundOption(..))


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
