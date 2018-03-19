module Replay.State exposing (..)

import GameState.Decoders exposing (playStateDecoder)
import GameState.State exposing (resolvable, resMapPlay)
import GameState.Types exposing (PlayState(..))
import Json.Decode as Json
import Main.Messages as Main
import Model.Decoders as Model
import Model.Types exposing (Model)
import Model.ViewModel
import Replay.Messages exposing (Msg(..))
import Replay.Types as Replay
import Resolvable.Decoders exposing (resolveDiffDataDecoder)
import Resolvable.State as Resolvable
import Resolvable.Types as Resolvable
import Room.Messages as Room
import Util exposing (message, splitOn, unsafeForceDecode)


init : Replay.Model
init =
    { replay = Nothing
    }


receive : String -> Cmd Main.Msg
receive msg =
    let
        ( command, content ) =
            splitOn ":" msg
    in
        case command of
            "replay" ->
                let
                    initial : Model
                    initial =
                        unsafeForceDecode
                            ((Json.field "initial") Model.decoder)
                            content

                    resDiffList : List Resolvable.ResolveDiffData
                    resDiffList =
                        unsafeForceDecode
                            ((Json.field "list") (Json.list resolveDiffDataDecoder))
                            content

                    resList : List Resolvable.ResolveData
                    resList =
                        Resolvable.resDiffToData initial resDiffList

                    finalState : PlayState
                    finalState =
                        unsafeForceDecode
                            ((Json.field "final") playStateDecoder)
                            content

                    model : Model
                    model =
                        .final <| resolvable finalState

                    res : Resolvable.Model
                    res =
                        { vm = Model.ViewModel.init
                        , tick = 0
                        , final = model
                        , resList = resList
                        }

                    replay : PlayState
                    replay =
                        resMapPlay (\_ -> res) finalState
                in
                    message <|
                        Main.RoomMsg <|
                            Room.ReplayMsg <|
                                SetReplay replay

            otherwise ->
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


tick : Replay.Model -> Float -> Replay.Model
tick model dt =
    let
        newReplay =
            Maybe.map (resMapPlay (Resolvable.tick dt)) model.replay
    in
        { model | replay = newReplay }
