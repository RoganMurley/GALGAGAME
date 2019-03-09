module Signup.State exposing (init, keyPress, receive, update, validator)

import Error exposing (Error(..))
import Http
import Json.Decode exposing (maybe)
import Keyboard exposing (KeyCode)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Maybe.Extra as Maybe
import Navigation
import Room.Messages as Room
import Signup.Decoders exposing (signupErrorDecoder)
import Signup.Messages exposing (Input(..), Msg(..))
import Signup.Types exposing (Field(..), Model)
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


usernameValidator : { a | username : String } -> Maybe ( Field, Error )
usernameValidator { username } =
    let
        tooShort : Maybe ( Field, Error )
        tooShort =
            if String.length username < 3 then
                Just ( UsernameField, Error "Username must be at least 3 characters long" )

            else
                Nothing

        tooLong : Maybe ( Field, Error )
        tooLong =
            if String.length username > 12 then
                Just ( UsernameField, Error "Username must not be longer than 12 characters" )

            else
                Nothing
    in
    Maybe.next tooShort tooLong


passwordValidator : { a | password : String } -> Maybe ( Field, Error )
passwordValidator { password } =
    if String.length password < 8 then
        Just ( PasswordField, Error "Password must be at least 8 characters" )

    else
        Nothing


confirmPasswordValidator : { a | confirmPassword : String, password : String } -> Maybe ( Field, Error )
confirmPasswordValidator { confirmPassword, password } =
    if confirmPassword /= password then
        Just ( ConfirmPasswordField, Error "Passwords do not match" )

    else
        Nothing


validator : Model -> Maybe ( Field, Error )
validator model =
    List.foldl Maybe.or
        Nothing
    <|
        List.map (\v -> v model)
            [ usernameValidator, passwordValidator, confirmPasswordValidator ]
