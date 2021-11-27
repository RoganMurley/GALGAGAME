module Chat.State exposing (init, keyPress, update)

import Assets.Types as Assets
import Audio.State exposing (playSound)
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
    , notify = False
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


update : Msg -> Model -> Assets.Model -> ( Model, Cmd Main.Msg )
update msg model { audio } =
    case msg of
        RecvMessage newMessage ->
            let
                messages =
                    newMessage :: model.messages

                notify =
                    not model.visible

                cmd =
                    if notify then
                        playSound audio "sfx/notify.mp3"

                    else
                        Cmd.none
            in
            ( { model | messages = messages, notify = notify }, cmd )

        SendMessage newMessage ->
            if newMessage /= "" then
                ( { model | input = "" }, Ports.websocketSend <| "chat:" ++ newMessage )

            else
                ( model, Cmd.none )

        ToggleVisibility ->
            ( { model | visible = not model.visible, notify = False }, Cmd.none )

        SetInput input ->
            ( { model | input = input }, Cmd.none )
