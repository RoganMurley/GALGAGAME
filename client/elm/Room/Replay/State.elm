module Replay.State exposing (init, mouseDown, mouseUp, tick, update)

import Assets.Types as Assets
import Chat.State as Chat
import GameType exposing (GameType(..))
import Http
import Main.Messages as Main
import Main.Types exposing (Flags)
import Math.Vector2
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
    , playing = False
    , speed = 1
    , frame = 0
    , pos = { x = 0, y = 0 }
    , drag = Nothing
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

        SetPlaying playing ->
            ( { model | playing = playing }
            , Cmd.none
            )

        SpeedUp ->
            if model.speed < 16 then
                ( { model | speed = model.speed * 1.1 }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        SlowDown ->
            if model.speed > 0.626 then
                ( { model | speed = model.speed / 1.1 }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

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

        NoOp ->
            ( model, Cmd.none )


tick : Flags -> Replay.Model -> Float -> Replay.Model
tick flags model dtRaw =
    let
        newPos =
            case model.drag of
                Just drag ->
                    Maybe.withDefault model.pos
                        (Mouse.getVec flags.mouse
                            |> Maybe.map
                                (Math.Vector2.toRecord
                                    >> (\{ x, y } ->
                                            { x = floor x + drag.x
                                            , y = floor y + drag.y
                                            }
                                       )
                                )
                        )

                _ ->
                    model.pos
    in
    if model.started then
        let
            dt =
                dtRaw * model.speed

            replay =
                if model.playing then
                    Maybe.map
                        (\r ->
                            { r
                                | state =
                                    Tuple.first <| PlayState.tick flags r.state Chat.init CustomGame dt
                                , tick = r.tick + dt
                            }
                        )
                        model.replay

                else
                    model.replay
        in
        { model
            | replay = replay
            , frame =
                if model.playing then
                    model.frame + dt

                else
                    model.frame
            , pos = newPos
        }

    else
        { model | pos = newPos }


mouseDown : Flags -> Assets.Model -> Replay.Model -> Mouse.Position -> ( Replay.Model, Cmd Main.Msg )
mouseDown _ _ model _ =
    -- Require a click to play a replay so that audio can be played.
    -- If we don't do that the sounds may all play at once later.
    if model.started then
        ( model, Cmd.none )

    else
        ( { model | started = True, playing = True }, Cmd.none )


mouseUp : Flags -> Replay.Model -> Replay.Model
mouseUp _ model =
    { model
        | drag = Nothing
    }
