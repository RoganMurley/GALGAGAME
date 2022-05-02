module Profile.State exposing (init, update)

import Http
import Main.Messages as Main
import Main.Types exposing (Flags)
import Profile.Decoders as Profile
import Profile.Messages exposing (Msg(..))
import Profile.Types exposing (Model)
import Room.Messages as Room
import Util exposing (apiLocation)


init : Model
init =
    { error = ""
    , profile = Nothing
    }


update : Model -> Msg -> Flags -> ( Model, Cmd Main.Msg )
update model msg flags =
    case msg of
        Load username ->
            ( model
            , Http.get
                { url = apiLocation flags ++ "/profile/" ++ username
                , expect =
                    Http.expectJson
                        (Main.RoomMsg << Room.ProfileMsg << LoadCallback)
                        Profile.decoder
                }
            )

        LoadCallback (Ok profile) ->
            ( { model | profile = Just profile }, Cmd.none )

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
