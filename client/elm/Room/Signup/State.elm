module Signup.State exposing (init, keyPress, receive, update, validator)

import Browser.Navigation
import Form exposing (Error(..), ValidationResult, Validator, batchValidators, initFormField, updateFormField)
import Http
import Json.Decode exposing (maybe)
import Keyboard exposing (Key(..))
import Main.Messages as Main
import Main.Types exposing (Flags)
import Regex exposing (Regex)
import Room.Messages as Room
import Signup.Decoders exposing (signupErrorDecoder)
import Signup.Messages exposing (Msg(..))
import Signup.Types exposing (Field(..), Model)
import Util exposing (apiLocation, message)


init : Maybe String -> Model
init nextUrl =
    { email = initFormField
    , username = initFormField
    , password = initFormField
    , contactable = initFormField
    , error = ""
    , submitting = False
    , nextUrl = Maybe.withDefault "/play" nextUrl
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

        Input Contactable contactable ->
            ( { model | contactable = updateFormField contactable model.contactable }, Cmd.none )

        Submit ->
            ( { model | submitting = True, error = "" }
            , Http.post
                { url = apiLocation flags ++ "/register"
                , body =
                    Http.multipartBody
                        [ Http.stringPart "email" model.email.value
                        , Http.stringPart "username" model.username.value
                        , Http.stringPart "password" model.password.value
                        , Http.stringPart "contactable" model.contactable.value
                        ]
                , expect =
                    Http.expectJson (Main.RoomMsg << Room.SignupMsg << SubmitCallback) (maybe signupErrorDecoder)
                }
            )

        SubmitCallback (Ok (Just { error })) ->
            ( { model | error = error }, Cmd.none )

        SubmitCallback (Ok Nothing) ->
            ( model
            , Cmd.batch
                [ message <| Main.SetUsername model.username.value
                , Browser.Navigation.load model.nextUrl
                ]
            )

        SubmitCallback (Err httpError) ->
            case httpError of
                Http.BadStatus status ->
                    case status of
                        409 ->
                            ( { model
                                | error = "Account with that username already exists"
                                , submitting = False
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( { model
                                | error = String.fromInt status
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


keyPress : Key -> Cmd Main.Msg
keyPress code =
    case code of
        EnterKey ->
            message <| Main.RoomMsg <| Room.SignupMsg <| Submit


emailValidator : Validator Model Field
emailValidator =
    let
        required : Validator Model Field
        required { email } =
            if String.length email.value == 0 then
                [ { field = Email
                  , error = Error "Required"
                  , touched = email.touched
                  }
                ]

            else
                []

        valid : Validator Model Field
        valid { email } =
            if not (Regex.contains validEmail email.value) then
                [ { field = Email
                  , error = Error "Invalid email"
                  , touched = email.touched
                  }
                ]

            else
                []

        validEmail : Regex
        validEmail =
            Maybe.withDefault Regex.never <|
                Regex.fromStringWith
                    { caseInsensitive = False, multiline = False }
                    "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
    in
    batchValidators [ required, valid ]


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

        valid : Validator Model Field
        valid { username } =
            if not (Regex.contains validUsername username.value) then
                [ { field = Username
                  , error = Error "Illegal characters"
                  , touched = username.touched
                  }
                ]

            else
                []

        validUsername : Regex
        validUsername =
            Maybe.withDefault Regex.never <|
                Regex.fromStringWith
                    { caseInsensitive = False, multiline = False }
                    "^[a-zA-Z0-9\\-\\_\\.]*$"
    in
    batchValidators [ required, tooShort, tooLong, valid ]


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
