module Signup.State exposing (init, keyPress, receive, update, validator)

import Form exposing (Error(..), ValidationResult, Validator, batchValidators, initFormField, updateFormField)
import Http
import Json.Decode exposing (maybe)
import Keyboard exposing (KeyCode)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Navigation
import Room.Messages as Room
import Signup.Decoders exposing (signupErrorDecoder)
import Signup.Messages exposing (Msg(..))
import Signup.Types exposing (Field(..), Model)
import Util exposing (authLocation, message, send)


init : Maybe String -> Model
init nextUrl =
    { email = initFormField
    , username = initFormField
    , password = initFormField
    , error = ""
    , submitting = False
    , nextUrl = Maybe.withDefault "/" nextUrl
    }


update : Model -> Msg -> Flags -> ( Model, Cmd Main.Msg )
update model msg flags =
    case msg of
        Input Email email ->
            ( { model | email = updateFormField email model.email }, Cmd.none )

        Input Username username ->
            ( { model | username = updateFormField username model.username }, Cmd.none )

        Input Password password ->
            ( { model | password = updateFormField password model.password }, Cmd.none )

        Submit ->
            ( { model | submitting = True, error = "" }
            , Http.send
                (Main.RoomMsg << Room.SignupMsg << SubmitCallback)
              <|
                Http.post
                    (authLocation flags ++ "/register")
                    (Http.multipartBody
                        [ Http.stringPart "email" model.email.value
                        , Http.stringPart "username" model.username.value
                        , Http.stringPart "password" model.password.value
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


emailValidator : Validator Model Field
emailValidator { email } =
    if String.length email.value == 0 then
        [ { field = Email
          , error = Error "Required"
          , touched = email.touched
          }
        ]

    else
        []


usernameValidator : Validator Model Field
usernameValidator =
    let
        required : Validator Model Field
        required { username } =
            if String.length username.value == 0 then
                [ { field = Username
                  , error = Error "Required"
                  , touched = username.touched
                  }
                ]

            else
                []

        tooShort : Validator Model Field
        tooShort { username } =
            if String.length username.value < 3 then
                [ { field = Username
                  , error = Error "Too short"
                  , touched = username.touched
                  }
                ]

            else
                []

        tooLong : Validator Model Field
        tooLong { username } =
            if String.length username.value > 12 then
                [ { field = Username
                  , error = Error "Too long"
                  , touched = username.touched
                  }
                ]

            else
                []
    in
    batchValidators [ required, tooShort, tooLong ]


passwordValidator : Validator Model Field
passwordValidator =
    let
        required : Validator Model Field
        required { password } =
            if String.length password.value == 0 then
                [ { field = Password
                  , error = Error "Required"
                  , touched = password.touched
                  }
                ]

            else
                []

        tooShort : Validator Model Field
        tooShort { password } =
            if String.length password.value < 8 then
                [ { field = Password
                  , error = Error "Too short"
                  , touched = password.touched
                  }
                ]

            else
                []
    in
    batchValidators [ required, tooShort ]


validator : Model -> List (ValidationResult Field)
validator =
    batchValidators [ emailValidator, usernameValidator, passwordValidator ]
