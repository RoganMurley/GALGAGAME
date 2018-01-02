module Login.State exposing (..)

import Http
import Json.Decode exposing (maybe)
import Login.Decoders exposing (loginErrorDecoder)
import Login.Messages exposing (..)
import Login.Types exposing (..)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Navigation
import Room.Messages as Room
import Util exposing (authLocation, message, send)


init : Maybe String -> Model
init nextUrl =
    { username = ""
    , password = ""
    , error = ""
    , submitting = False
    , nextUrl = Maybe.withDefault "/" nextUrl
    }


update : Model -> Msg -> Flags -> ( Model, Cmd Main.Msg )
update model msg flags =
    case msg of
        Input Username username ->
            ( { model | username = username }, Cmd.none )

        Input Password password ->
            ( { model | password = password }, Cmd.none )

        Submit ->
            ( { model | submitting = True, error = "" }
            , Http.send
                (Main.RoomMsg << Room.LoginMsg << SubmitCallback)
              <|
                Http.post
                    ((authLocation flags) ++ "/login")
                    (Http.multipartBody
                        [ Http.stringPart "username" model.username
                        , Http.stringPart "password" model.password
                        ]
                    )
                    (maybe loginErrorDecoder)
            )

        SubmitCallback (Ok (Just { error })) ->
            ( { model | error = error }, Cmd.none )

        SubmitCallback (Ok Nothing) ->
            ( model
            , Cmd.batch
                [ Navigation.newUrl model.nextUrl
                , message Main.GetAuth

                -- Reconnect so that the ws connection has our login cookie
                , send flags "reconnect:"
                ]
            )

        SubmitCallback (Err httpError) ->
            case httpError of
                Http.BadStatus { status } ->
                    case status.code of
                        401 ->
                            ( { model
                                | error = "Bad username/password"
                                , submitting = False
                              }
                            , Cmd.none
                            )

                        otherwise ->
                            ( { model
                                | error = status.message
                                , submitting = False
                              }
                            , Cmd.none
                            )

                otherwise ->
                    ( { model
                        | error = "Error connecting to authentication service"
                        , submitting = False
                      }
                    , Cmd.none
                    )


receive : String -> Cmd Main.Msg
receive _ =
    Cmd.none


usernameInvalid : String -> Bool
usernameInvalid username =
    (String.length username < 3) || (String.length username > 12)


passwordInvalid : String -> Bool
passwordInvalid password =
    (String.length password < 3) || (String.length password > 12)
