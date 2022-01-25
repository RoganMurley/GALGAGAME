module Settings.State exposing (close, init, update)

import Settings.Messages exposing (Msg(..))
import Settings.Types exposing (ModalState(..), Model, VolumeType(..))


init : Int -> Int -> Model
init volume musicVolume =
    { modalState = Closed
    , musicVolume = musicVolume
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

                ModalOpen ->
                    close m

                MenuOpen ->
                    close m

        OpenSettings ->
            openModal m

        Close ->
            close m

        OpenMenu ->
            openMenu m

        SetGameSpeed gameSpeed ->
            { m | gameSpeed = gameSpeed }


openModal : Model -> Model
openModal m =
    { m | modalState = ModalOpen }


openMenu : Model -> Model
openMenu m =
    { m | modalState = MenuOpen }


close : Model -> Model
close m =
    { m | modalState = Closed }
