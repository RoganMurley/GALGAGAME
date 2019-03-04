module Signup.State exposing (confirmPasswordInvalid, init, keyPress, receive, submitDisabled, update)

import Http
import Json.Decode exposing (maybe)
import Keyboard exposing (KeyCode)
import Login.State exposing (passwordInvalid, usernameInvalid)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Navigation
import Room.Messages as Room
import Signup.Decoders exposing (signupErrorDecoder)
import Signup.Messages exposing (Input(..), Msg(..))
import Signup.Types exposing (Model)
import Util exposing (authLocation, message, send)


init : Maybe String -> Model
init nextUrl =
    { email = ""
    , username = ""
    , password = ""
    , confirmPassword = ""
    , error = ""
    , submitting = False
    , nextUrl = Maybe.withDefault "/" nextUrl
    }


update : Model -> Msg -> Flags -> ( Model, Cmd Main.Msg )
update model msg flags =
    case msg of
        Input Email email ->
            ( { model | email = email }, Cmd.none )

        Input Username username ->
            ( { model | username = username }, Cmd.none )

        Input Password password ->
            ( { model | password = password }, Cmd.none )

        Input ConfirmPassword password ->
            ( { model | confirmPassword = password }, Cmd.none )

        Submit ->
            if submitDisabled model then
                ( model, Cmd.none )

            else
                ( { model | submitting = True, error = "" }
                , Http.send
                    (Main.RoomMsg << Room.SignupMsg << SubmitCallback)
                  <|
                    Http.post
                        (authLocation flags ++ "/register")
                        (Http.multipartBody
                            [ Http.stringPart "email" model.email
                            , Http.stringPart "username" model.username
                            , Http.stringPart "password" model.password
                            ]
                        )
                        (maybe signupErrorDecoder)
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
                        409 ->
                            ( { model
                                | error = "Account with that username already exists"
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


receive : String -> Cmd Main.Msg
receive _ =
    Cmd.none


keyPress : KeyCode -> Cmd Main.Msg
keyPress code =
    case code of
        -- Enter key
        13 ->
            message <| Main.RoomMsg <| Room.SignupMsg <| Submit

        _ ->
            Cmd.none


confirmPasswordInvalid : { a | confirmPassword : String, password : String } -> Bool
confirmPasswordInvalid { confirmPassword, password } =
    confirmPassword /= password


submitDisabled : Model -> Bool
submitDisabled model =
    usernameInvalid model
        || passwordInvalid model
        || confirmPasswordInvalid model
        || model.submitting
