module Presence.State exposing (init, update)

import Http
import Main.Messages as Main
import Main.Types exposing (Flags)
import Presence.Decoders as Presence
import Presence.Messages exposing (Msg(..))
import Presence.Types exposing (Model)
import Room.Messages as Room
import Util exposing (apiLocation)


init : Model
init =
    { error = ""
    , presence = Nothing
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
                        (Main.RoomMsg << Room.PresenceMsg << LoadCallback)
                        Presence.decoder
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
