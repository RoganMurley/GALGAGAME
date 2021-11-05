module Settings.State exposing (init, update)

import Settings.Messages exposing (Msg(..))
import Settings.Types exposing (ModalState(..), Model, VolumeType(..))


init : Int -> Model
init volume =
    { modalState = Closed
    , masterVolume = volume
    , musicVolume = volume
    , sfxVolume = volume
    , gameSpeed = 1
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

        SetGameSpeed gameSpeed ->
            { m | gameSpeed = gameSpeed }


openModal : Model -> Model
openModal m =
    { m | modalState = Open }


closeModal : Model -> Model
closeModal m =
    { m | modalState = Closed }
