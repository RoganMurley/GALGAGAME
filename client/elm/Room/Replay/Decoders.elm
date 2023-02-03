module Replay.Decoders exposing (dragEventDecoder, replayDecoder)

import Drag exposing (Drag(..))
import Json.Decode as Json exposing (Decoder)
import Model.Types exposing (Model)
import PlayState.Decoders exposing (resolveOutcomeInputDecoder)
import PlayState.State as PlayState
import PlayState.Types exposing (PlayState, ResolveOutcomeInput)
import Replay.Messages exposing (Msg(..))
import Replay.Types exposing (Replay)
import Resolvable.State as Resolvable
import Resolvable.Types as Resolvable


replayDecoder : Decoder Replay
replayDecoder =
    let
        makeReplayState : ResolveOutcomeInput -> PlayState
        makeReplayState { resDiffList, initial, finalState } =
            let
                resList : List Resolvable.ResolveData
                resList =
                    Resolvable.resDiffToData initial resDiffList

                model : Model
                model =
                    PlayState.get (.final << .res) finalState

                res : Resolvable.Model
                res =
                    { tick = 0
                    , final = model
                    , resList = resList
                    , history = []
                    }
            in
            PlayState.map (\game -> { game | res = res }) finalState

        replayStateDecoder : Decoder PlayState
        replayStateDecoder =
            resolveOutcomeInputDecoder
                |> Json.andThen
                    (\resolveOutcomeInput ->
                        Json.succeed <| makeReplayState resolveOutcomeInput
                    )
    in
    Json.map4 Replay
        replayStateDecoder
        (Json.field "pa" Json.string)
        (Json.field "pb" Json.string)
        (Json.succeed 0)


dragEventDecoder : Decoder Msg
dragEventDecoder =
    let
        toMsg : Drag -> Msg
        toMsg drag =
            case drag of
                Drag pos ->
                    DragStart pos

                NoDrag ->
                    NoOp
    in
    Json.map toMsg Drag.decoder
