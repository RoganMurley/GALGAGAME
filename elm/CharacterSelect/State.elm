module CharacterSelect.State exposing (..)

import CharacterSelect.Types exposing (Model)
import CharacterSelect.Messages exposing (Msg(..))


update : Msg -> Model -> Model
update msg model =
    case msg of
        Hover character ->
            { model | hover = character }
