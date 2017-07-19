module Settings.State exposing (init, update)

import Settings.Messages exposing (Msg(..))
import Settings.Types exposing (Model(..))


init : Model
init =
    Closed


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleSettings ->
            case model of
                Closed ->
                    Open

                Open ->
                    Closed

        OpenSettings ->
            Open

        CloseSettings ->
            Closed
