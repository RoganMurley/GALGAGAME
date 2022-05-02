module Leaderboard.State exposing (init, update)

import Http
import Leaderboard.Decoders as Leaderboard
import Leaderboard.Messages exposing (Msg(..))
import Leaderboard.Types exposing (Model)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Room.Messages as Room
import Util exposing (apiLocation)


init : Model
init =
    { error = ""
    , entries = []
    }


update : Model -> Msg -> Flags -> ( Model, Cmd Main.Msg )
update model msg flags =
    case msg of
        Load ->
            ( model
            , Http.get
                { url = apiLocation flags ++ "/leaderboard"
                , expect =
                    Http.expectJson
                        (Main.RoomMsg << Room.LeaderboardMsg << LoadCallback)
                        Leaderboard.decoder
                }
            )

        LoadCallback (Ok entries) ->
            ( { model | entries = entries }, Cmd.none )

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
