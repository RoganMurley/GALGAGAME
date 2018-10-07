module Settings.State exposing (init, update)

import Settings.Messages exposing (Msg(..))
import Settings.Types exposing (Model, ModalState(..), VolumeType(..))


init : Model
init =
    { modalState = Closed
    , masterVolume = 100
    , musicVolume = 100
    , sfxVolume = 100
    }


update : Msg -> Model -> Model
update msg m =
    case msg of
        ToggleSettings ->
            case m.modalState of
                Closed ->
                    openModal m

                Open ->
                    closeModal m

        OpenSettings ->
            openModal m

        CloseSettings ->
            closeModal m


openModal : Model -> Model
openModal m =
    { m | modalState = Open }


closeModal : Model -> Model
closeModal m =
    { m | modalState = Closed }
