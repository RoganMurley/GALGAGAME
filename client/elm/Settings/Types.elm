module Settings.Types exposing (ModalState(..), Model, VolumeType(..))


type alias Model =
    { modalState : ModalState
    , musicVolume : Int
    , sfxVolume : Int
    , gameSpeed : Float
    }


type VolumeType
    = Music
    | Sfx


type ModalState
    = Open
    | Closed
