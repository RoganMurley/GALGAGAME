module Profile.State exposing (init, receive, update)

import Http
import Main.Messages as Main
import Main.Types exposing (Flags)
import Ports exposing (log)
import Profile.Decoders as Profile
import Profile.Messages exposing (Msg(..))
import Profile.Types exposing (Model)
import Room.Messages as Room
import Util exposing (apiLocation, message, splitOnColon)


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
            ( { model | profile = Just { profile | isMe = flags.username == Just profile.name } }, Cmd.none )

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
