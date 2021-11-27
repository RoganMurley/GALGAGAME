module Chat.State exposing (init, update)

import Chat.Messages exposing (Msg(..))
import Chat.Types exposing (Model)
import Main.Messages as Main


init : Model
init =
    { messages = [], visible = True }


update : Msg -> Model -> ( Model, Cmd Main.Msg )
update msg model =
    case msg of
        RecvMessage newMessage ->
            ( { model | messages = newMessage :: model.messages }, Cmd.none )

        SetVisibility visible ->
            ( { model | visible = visible }, Cmd.none )
