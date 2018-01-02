module Settings.Types exposing (..)


type alias Model =
    { modalState : ModalState
    , volume : Int
    }


type ModalState
    = Open
    | Closed
