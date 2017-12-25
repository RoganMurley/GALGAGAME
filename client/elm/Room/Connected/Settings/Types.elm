module Settings.Types exposing (..)

import Connected.Messages as Connected


type alias Model =
    { modalState : ModalState
    , volume : Int
    }


type ModalState
    = Open
    | Closed
