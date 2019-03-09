module Login.State exposing (init, keyPress, receive, update, validator)

import Form exposing (Error(..), validate)
import Http
import Json.Decode exposing (maybe)
import Keyboard exposing (KeyCode)
import Login.Decoders exposing (loginErrorDecoder)
import Login.Messages exposing (Msg(..))
import Login.Types exposing (Field(..), Model)
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
                    (authLocation flags ++ "/login")
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
            model
                ! [ Navigation.newUrl model.nextUrl
                  , message Main.GetAuth

                  -- Reconnect so that the ws connection has our login cookie
                  , send flags "reconnect:"
                  ]

        SubmitCallback (Err httpError) ->
            case httpError of
                Http.BadStatus { status } ->
                    case status.code of
                        401 ->
                            ( { model
                                | error = "Bad username or password"
                                , submitting = False
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( { model
                                | error = status.message
                                , submitting = False
                              }
                            , Cmd.none
                            )

                _ ->
                    ( { model
                        | error = "Error connecting to authentication service"
                        , submitting = False
                      }
                    , Cmd.none
                    )


keyPress : KeyCode -> Cmd Main.Msg
keyPress code =
    case code of
        -- Enter key
        13 ->
            message <| Main.RoomMsg <| Room.LoginMsg <| Submit

        _ ->
            Cmd.none


receive : String -> Cmd Main.Msg
receive _ =
    Cmd.none


usernameValidator : { a | username : String } -> Maybe ( Field, Error )
usernameValidator { username } =
    if String.length username == 0 then
        Just ( Username, Error "Enter a username" )

    else
        Nothing


passwordValidator : { a | password : String } -> Maybe ( Field, Error )
passwordValidator { password } =
    if String.length password == 0 then
        Just ( Password, Error "Enter a password" )

    else
        Nothing


validator : Model -> Maybe ( Field, Error )
validator =
    validate [ passwordValidator, usernameValidator ]
