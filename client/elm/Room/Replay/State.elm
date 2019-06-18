module Replay.State exposing (getReplay, init, receive, tick, update)

import Json.Decode as Json
import Main.Messages as Main
import Main.Types exposing (Flags)
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
    }


receive : String -> Cmd Main.Msg
receive msg =
    let
        ( command, content ) =
            splitOnColon msg
    in
    case command of
        "replay" ->
            case Json.decodeString replayDecoder content of
                Ok replay ->
                    message <|
                        Main.RoomMsg <|
                            Room.ReplayMsg <|
                                SetReplay replay

                Err err ->
                    log <| Json.errorToString err

        _ ->
            Cmd.none


update : Replay.Model -> Msg -> Replay.Model
update model msg =
    case msg of
        SetReplay replay ->
            { model | replay = Just replay }


getReplay : String -> Cmd Main.Msg
getReplay replayId =
    message <|
        Main.Send <|
            "playReplay:"
                ++ replayId


tick : Flags -> Replay.Model -> Float -> Replay.Model
tick flags model dt =
    let
        replay =
            Maybe.map
                (\r ->
                    { r
                        | state =
                            Tuple.first <| PlayState.tick flags r.state dt
                        , tick = r.tick + dt
                    }
                )
                model.replay
    in
    { model | replay = replay }
