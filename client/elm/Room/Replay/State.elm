module Replay.State exposing (init, mouseDown, tick, update)

import Assets.Types as Assets
import Chat.State as Chat
import GameType exposing (GameType(..))
import Http
import Main.Messages as Main
import Main.Types exposing (Flags)
import Mouse
import PlayState.Decoders as PlayState
import PlayState.State as PlayState
import PlayState.Types exposing (PlayState(..))
import Replay.Decoders exposing (replayDecoder)
import Replay.Messages exposing (Msg(..))
import Replay.Types as Replay
import Room.Messages as Room
import Tuple
import Util exposing (apiLocation)


init : Replay.Model
init =
    { replay = Nothing
    , started = False
    , error = ""
    }


update : Replay.Model -> Msg -> Flags -> ( Replay.Model, Cmd Main.Msg )
update model msg flags =
    case msg of
        Load replayId ->
            ( model
            , Http.get
                { url = apiLocation flags ++ "/replay/" ++ replayId
                , expect =
                    Http.expectJson
                        (Main.RoomMsg << Room.ReplayMsg << LoadCallback)
                        replayDecoder
                }
            )

        LoadCallback (Ok replay) ->
            ( { model | replay = Just replay }, Cmd.none )

        LoadCallback (Err err) ->
            let
                error =
                    case err of
                        Http.BadStatus 404 ->
                            "Replay not found"

                        Http.BadStatus status ->
                            "Error connecting to server (status: " ++ String.fromInt status ++ ")"

                        _ ->
                            "Error connecting to server"
            in
            ( { model | error = error }
            , Cmd.none
            )


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
