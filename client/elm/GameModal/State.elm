module GameModal.State exposing (init, update)

import GameModal.Messages exposing (Msg(..))
import GameModal.Types exposing (Model(..))


init : Model
init =
    Closed


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleModal ->
            case model of
                Closed ->
                    Open

                Open ->
                    Closed
