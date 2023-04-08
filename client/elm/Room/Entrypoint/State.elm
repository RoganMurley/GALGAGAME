module Entrypoint.State exposing (init, receive, tick, update)

import Entrypoint.Decoders as Entrypoint
import Entrypoint.Messages exposing (Msg(..))
import Entrypoint.Types exposing (Model)
import Http
import Main.Messages as Main
import Main.Types exposing (Flags)
import Ports exposing (log, websocketSend)
import Room.Generators exposing (generate)
import Room.Messages as Room
import Util exposing (apiLocation, message, splitOnColon)


maxTimer : Float
maxTimer =
    6000


init : Model
init =
    { error = ""
    , presence = Nothing
    , timer = maxTimer
    }


update : Model -> Msg -> Flags -> ( Model, Cmd Main.Msg )
update model msg flags =
    case msg of
        Load ->
            ( model
            , Http.get
                { url = apiLocation flags ++ "/presence"
                , expect =
                    Http.expectJson
                        (Main.RoomMsg << Room.EntrypointMsg << LoadCallback)
                        Entrypoint.decoder
                }
            )

        LoadCallback (Ok presence) ->
            ( { model | presence = Just presence }, Cmd.none )

        LoadCallback (Err err) ->
            let
                statusStr =
                    case err of
                        Http.BadStatus status ->
                            " (status: " ++ String.fromInt status ++ ")"

                        _ ->
                            ""
            in
            ( { model | error = "Error connecting to server" ++ statusStr }
            , Cmd.none
            )

        Challenge uid ->
            let
                roomId =
                    generate Room.Generators.roomID flags.seed
            in
            ( model
            , websocketSend <| "challenge:" ++ String.fromInt uid ++ "," ++ roomId
            )

        Quickplay ->
            ( model, message Main.GotoQuickplayGame )

        CustomGame ->
            ( model, message <| Main.GotoCustomGame Nothing )


receive : String -> Cmd Main.Msg
receive msg =
    let
        ( command, content ) =
            splitOnColon msg
    in
    case command of
        "challengeRoom" ->
            message <| Main.GotoChallengeGame (Just content)

        _ ->
            log <| "Error decoding message from server: " ++ msg


tick : Float -> Model -> ( Model, Cmd Msg )
tick dt model =
    if model.timer - dt < 0 then
        ( { model | timer = maxTimer }
        , message Load
        )

    else
        ( { model | timer = model.timer - dt }
        , Cmd.none
        )
