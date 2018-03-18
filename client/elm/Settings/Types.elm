module Settings.Types exposing (..)


type alias Model =
    { modalState : ModalState
    , masterVolume : Int
    , musicVolume : Int
    , sfxVolume : Int
    }


type VolumeType
    = Master
    | Music
    | Sfx


type ModalState
    = Open
    | Closed
