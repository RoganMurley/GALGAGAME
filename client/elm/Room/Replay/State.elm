module Replay.State exposing (init, mouseDown, mouseUp, tick, update)

import Assets.Types as Assets
import Browser.Navigation
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
import Replay.Types as Replay exposing (Replay)
import Resolvable.State as Resolvable
import Room.Messages as Room
import Tuple
import Util exposing (apiLocation)


init : String -> Float -> Replay.Model
init id frame =
    { replay = Nothing
    , started = frame > 0
    , error = ""
    , playing = False
    , speed = 1
    , frame = frame
    , pos = { x = 0, y = 0 }
    , drag = Nothing
    , id = id
    , reverse = False
    }


update : Replay.Model -> Msg -> Flags -> ( Replay.Model, Cmd Main.Msg )
update model msg flags =
    case msg of
        Load replayId frame ->
            ( model
            , Http.get
                { url = apiLocation flags ++ "/replay/" ++ replayId
                , expect =
                    Http.expectJson
                        (Main.RoomMsg << Room.ReplayMsg << LoadCallback frame)
                        replayDecoder
                }
            )

        LoadCallback frame (Ok replay) ->
            ( { model | replay = Just <| goto frame replay, frame = frame }, Cmd.none )

        LoadCallback _ (Err err) ->
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
            case model.replay of
                Just _ ->
                    ( { model | playing = playing }
                    , Browser.Navigation.replaceUrl flags.key <|
                        "/replay/"
                            ++ model.id
                            ++ "?t="
                            ++ String.fromInt (floor model.frame)
                    )

                Nothing ->
                    ( model, Cmd.none )

        SpeedUp ->
            ( { model | speed = model.speed * 1.1 }
            , Cmd.none
            )

        SlowDown ->
            ( { model | speed = model.speed / 1.1 }
            , Cmd.none
            )

        SetReverse reverse ->
            ( { model | reverse = reverse }, Cmd.none )

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
                if model.reverse then
                    -dtRaw * model.speed

                else if model.playing then
                    dtRaw * model.speed

                else
                    0

            replay =
                if model.started then
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
                if model.playing || model.reverse then
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


goto : Float -> Replay -> Replay
goto frame replay =
    let
        f =
            \game -> { game | res = Resolvable.goto frame game.res }
    in
    { replay | state = PlayState.map f replay.state }
