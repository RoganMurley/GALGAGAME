module League.State exposing (init, receive, update)

import Browser.Navigation
import Form exposing (Error(..))
import Http
import Json.Decode exposing (maybe)
import Keyboard exposing (Key(..))
import League.Decoders exposing (leagueErrorDecoder)
import League.Messages exposing (Msg(..))
import League.Types exposing (Model, SubmitState(..))
import Login.Messages as Login
import Main.Messages as Main
import Main.Types exposing (Flags)
import Room.Messages as Room
import Util exposing (apiLocation, message)


init : Model
init =
    { error = ""
    , submitState = Waiting
    }


update : Model -> Msg -> Flags -> ( Model, Cmd Main.Msg )
update model msg flags =
    case msg of
        Submit ->
            ( { model | submitState = Submitting, error = "" }
            , Http.post
                { url =
                    apiLocation flags ++ "/league"
                , body = Http.emptyBody
                , expect =
                    Http.expectJson
                        (Main.RoomMsg << Room.LeagueMsg << SubmitCallback)
                        (maybe leagueErrorDecoder)
                }
            )

        SubmitCallback (Ok (Just { error })) ->
            ( { model | error = error, submitState = NotSubmitted }, Cmd.none )

        SubmitCallback (Ok Nothing) ->
            ( { model | submitState = Submitted }, Cmd.none )

        SubmitCallback (Err _) ->
            ( { model
                | error = "Error connecting to server"
                , submitState = NotSubmitted
              }
            , Cmd.none
            )

        CheckState ->
            ( { model | submitState = Waiting, error = "" }
            , Http.get
                { url = apiLocation flags ++ "/league"
                , expect =
                    Http.expectJson
                        (Main.RoomMsg << Room.LeagueMsg << CheckStateCallback)
                        (maybe leagueErrorDecoder)
                }
            )

        CheckStateCallback (Ok _) ->
            ( { model | submitState = Submitted }, Cmd.none )

        CheckStateCallback (Err (Http.BadStatus status)) ->
            case status of
                404 ->
                    ( { model | submitState = NotSubmitted }, Cmd.none )

                401 ->
                    ( model
                    , Cmd.batch
                        [ message <| Main.RoomMsg <| Room.LoginMsg <| Login.SetNextUrl "/league"
                        , Browser.Navigation.pushUrl flags.key "/login"
                        ]
                    )

                _ ->
                    ( { model | submitState = Waiting, error = "Error connecting to server" }
                    , Cmd.none
                    )

        CheckStateCallback (Err _) ->
            ( { model
                | error = "Error connecting to server"
                , submitState = NotSubmitted
              }
            , Cmd.none
            )


receive : String -> Cmd Main.Msg
receive _ =
    Cmd.none
