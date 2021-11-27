module Chat.State exposing (init, keyPress, update)

import Chat.Messages exposing (Msg(..))
import Chat.Types exposing (Model)
import Connected.Messages as Connected
import Keyboard exposing (Key(..))
import Main.Messages as Main
import Ports
import Room.Messages as Room
import Util exposing (message)


init : Model
init =
    { input = ""
    , messages = []
    , visible = False
    }


keyPress : Model -> Key -> Cmd Main.Msg
keyPress { input, visible } code =
    case code of
        EnterKey ->
            if visible then
                message <|
                    Main.RoomMsg <|
                        Room.ConnectedMsg <|
                            Connected.ChatMsg <|
                                SendMessage input

            else
                Cmd.none


update : Msg -> Model -> ( Model, Cmd Main.Msg )
update msg model =
    case msg of
        RecvMessage newMessage ->
            ( { model | messages = newMessage :: model.messages }, Cmd.none )

        SendMessage newMessage ->
            if newMessage /= "" then
                ( { model | input = "" }, Ports.websocketSend <| "chat:" ++ newMessage )

            else
                ( model, Cmd.none )

        ToggleVisibility ->
            ( { model | visible = not model.visible }, Cmd.none )

        SetInput input ->
            ( { model | input = input }, Cmd.none )
