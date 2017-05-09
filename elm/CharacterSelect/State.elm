module CharacterSelect.State exposing (..)

import CharacterSelect.Types exposing (Model)
import CharacterSelect.Messages as CharacterSelect
import Util exposing (fromJust)


update : CharacterSelect.Msg -> Model -> Model
update msg model =
    case msg of
        CharacterSelect.Hover n ->
            { model | hover = fromJust (List.head (List.filter (\{ name } -> name == n) model.characters)) }
