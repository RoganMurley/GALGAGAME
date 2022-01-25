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
        Close ->
            close m

        OpenMenu ->
            openMenu m

        SetGameSpeed gameSpeed ->
            { m | gameSpeed = gameSpeed }


openMenu : Model -> Model
openMenu m =
    { m | modalState = MenuOpen }


close : Model -> Model
close m =
    { m | modalState = Closed }
