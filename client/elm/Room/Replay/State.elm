module Replay.State exposing (getReplay, init, receive, tick, update)

import Json.Decode as Json
import Main.Messages as Main
import Main.Types exposing (Flags)
import Model.Decoders as Model
import Model.Types exposing (Model)
import Model.ViewModel
import PlayState.Decoders as PlayState
import PlayState.State as PlayState
import PlayState.Types exposing (PlayState(..))
import Replay.Messages exposing (Msg(..))
import Replay.Types as Replay exposing (Replay)
import Resolvable.Decoders
import Resolvable.State as Resolvable
import Resolvable.Types as Resolvable
import Room.Messages as Room
import Util exposing (message, splitOnColon, unsafeForceDecode)


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
            let
                initial : Model
                initial =
                    unsafeForceDecode
                        (Json.field "initial" Model.decoder)
                        content

                resDiffList : List Resolvable.ResolveDiffData
                resDiffList =
                    unsafeForceDecode
                        (Json.field "list" <|
                            Json.list Resolvable.Decoders.resolveDiffData
                        )
                        content

                resList : List Resolvable.ResolveData
                resList =
                    Resolvable.resDiffToData initial resDiffList

                finalState : PlayState
                finalState =
                    unsafeForceDecode
                        (Json.field "final" PlayState.decoder)
                        content

                model : Model
                model =
                    PlayState.get (.final << .res) finalState

                res : Resolvable.Model
                res =
                    { vm = Model.ViewModel.init
                    , tick = 0
                    , final = model
                    , resList = resList
                    }

                state : PlayState
                state =
                    PlayState.map (\game -> { game | res = res }) finalState

                usernamePa : String
                usernamePa =
                    unsafeForceDecode
                        (Json.field "pa" Json.string)
                        content

                usernamePb : String
                usernamePb =
                    unsafeForceDecode
                        (Json.field "pb" Json.string)
                        content

                replay : Replay
                replay =
                    { state = state
                    , usernamePa = usernamePa
                    , usernamePb = usernamePb
                    }
            in
            message <|
                Main.RoomMsg <|
                    Room.ReplayMsg <|
                        SetReplay replay

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
        newReplay =
            Maybe.map
                (\r -> { r | state = PlayState.tick flags r.state dt })
                model.replay
    in
    { model | replay = newReplay }
