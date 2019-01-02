module CharacterSelect.State exposing (getHoverSlot, update, viewModelInit)

import CharacterSelect.Messages exposing (Msg(..))
import CharacterSelect.Types exposing (Character, Model, Slot(..), ViewModel)
import Main.Messages as Main
import Util


update : Msg -> Model -> ( Model, Cmd Main.Msg )
update msg ({ vm } as m) =
    case msg of
        Hover character ->
            ( { m | vm = { vm | hover = character } }, Cmd.none )

        Select { name } ->
            let
                cmd =
                    Util.message <|
                        Main.Send <|
                            "selectCharacter:"
                                ++ name
            in
            ( m, cmd )


viewModelInit : Character -> ViewModel
viewModelInit character =
    { hover = character }


getHoverSlot : Model -> Maybe Slot
getHoverSlot { selected } =
    case List.length selected of
        0 ->
            Just SlotA

        1 ->
            Just SlotB

        2 ->
            Just SlotC

        _ ->
            Nothing
