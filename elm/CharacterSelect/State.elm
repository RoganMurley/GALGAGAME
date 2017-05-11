module CharacterSelect.State exposing (..)

import CharacterSelect.Types exposing (Model)
import CharacterSelect.Messages as CharacterSelect


update : CharacterSelect.Msg -> Model -> Model
update msg model =
    case msg of
        CharacterSelect.Hover character ->
            { model | hover = character }
