module Replay.State exposing (getReplay, init, mouseDown, receive, tick, update)

import Assets.Types as Assets
import Chat.State as Chat
import GameType exposing (GameType(..))
import Json.Decode as Json
import Main.Messages as Main
import Main.Types exposing (Flags)
import Mouse
import PlayState.Decoders as PlayState
import PlayState.State as PlayState
import PlayState.Types exposing (PlayState(..))
import Ports exposing (log)
import Replay.Decoders exposing (replayDecoder)
import Replay.Messages exposing (Msg(..))
import Replay.Types as Replay
import Room.Messages as Room
import Tuple
import Util exposing (message, splitOnColon)


init : Replay.Model
init =
    { replay = Nothing
    , started = False
    }


receive : String -> Cmd Main.Msg
receive msg =
    let
        ( command, content ) =
            splitOnColon msg
    in
    case command of
        "replay" ->
            message <|
                Main.RoomMsg <|
                    Room.ReplayMsg <|
                        SetReplay content

        _ ->
            Cmd.none


update : Replay.Model -> Msg -> ( Replay.Model, Cmd Main.Msg )
update model msg =
    case msg of
        SetReplay replayStr ->
            case Json.decodeString replayDecoder replayStr of
                Ok replay ->
                    ( { model | replay = Just replay }, Cmd.none )

                Err err ->
                    ( model, log <| Json.errorToString err )


getReplay : String -> Cmd Main.Msg
getReplay replayId =
    message <|
        Main.Send <|
            "playReplay:"
                ++ replayId


tick : Flags -> Replay.Model -> Float -> Replay.Model
tick flags model dt =
    if model.started then
        let
            replay =
                Maybe.map
                    (\r ->
                        { r
                            | state =
                                Tuple.first <| PlayState.tick flags r.state Chat.init CustomGame dt
                            , tick = r.tick + dt
                        }
                    )
                    model.replay
        in
        { model | replay = replay }

    else
        model


mouseDown : Flags -> Assets.Model -> Replay.Model -> Mouse.Position -> ( Replay.Model, Cmd Main.Msg )
mouseDown _ _ model _ =
    -- Require a click to play a replay so that audio can be played.
    -- If we don't do that the sounds may all play at once later.
    ( { model | started = True }, Cmd.none )
