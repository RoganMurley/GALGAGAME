module Chat.State exposing (init, keyPress, mouseUp, tick, update)

import Assets.Types as Assets
import Audio.State exposing (playSound)
import Browser.Dom
import Chat.Messages exposing (Msg(..))
import Chat.Types exposing (Model)
import Connected.Messages as Connected
import Keyboard exposing (Key(..))
import Main.Messages as Main
import Main.Types exposing (Flags)
import Math.Vector2
import Maybe
import Mouse
import Ports
import Room.Messages as Room
import Task
import Util exposing (message)


init : Model
init =
    { input = ""
    , messages = []
    , visible = False
    , notify = False
    , pos = { x = 0, y = 0 }
    , drag = Nothing
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


mouseUp : Flags -> Model -> Model
mouseUp _ model =
    { model
        | drag = Nothing
    }


tick : Flags -> Model -> Float -> Model
tick { mouse } model _ =
    case model.drag of
        Just drag ->
            let
                pos =
                    Mouse.getVec mouse
                        |> Maybe.map
                            (Math.Vector2.toRecord
                                >> (\{ x, y } ->
                                        { x = floor x + drag.x
                                        , y = floor y + drag.y
                                        }
                                   )
                            )
            in
            { model | pos = Maybe.withDefault model.pos pos }

        _ ->
            model


update : Msg -> Model -> Assets.Model -> ( Model, Cmd Main.Msg )
update msg model { audio } =
    case msg of
        DragStart pos ->
            ( { model
                | drag =
                    Just
                        { x = model.pos.x - pos.x
                        , y = model.pos.y - pos.y
                        }
              }
            , Cmd.none
            )

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
            let
                visible =
                    not model.visible

                cmd =
                    if visible then
                        Task.attempt
                            (\_ -> Main.NoOp)
                            (Browser.Dom.focus "chat-input")

                    else
                        Cmd.none
            in
            ( { model
                | visible = visible
                , notify = False
                , pos = { x = 0, y = 0 }
              }
            , cmd
            )

        SetInput input ->
            ( { model | input = input }, Cmd.none )
