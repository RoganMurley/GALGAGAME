module CharacterSelect.State exposing (..)

import CharacterSelect.Types exposing (Model)
import Messages exposing (CharSelectMsg(..))
import Util exposing (fromJust)


update : CharSelectMsg -> Model -> Model
update msg model =
    case msg of
        SelectingHover n ->
            { model | hover = fromJust (List.head (List.filter (\{ name } -> name == n) model.characters)) }
