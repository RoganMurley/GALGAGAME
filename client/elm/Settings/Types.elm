module Settings.Types exposing (ModalState(..), Model, VolumeType(..))


type alias Model =
    { modalState : ModalState
    , masterVolume : Int
    , musicVolume : Int
    , sfxVolume : Int
    , gameSpeed : Float
    }


type VolumeType
    = Master
    | Music
    | Sfx


type ModalState
    = Open
    | Closed
