module Login.State exposing (init, keyPress, receive, update, validator)

import Browser.Navigation
import Form exposing (Error(..), ValidationResult, Validator, batchValidators, initFormField, updateFormField)
import Http
import Json.Decode exposing (maybe)
import Keyboard exposing (Key(..))
import Login.Decoders exposing (loginErrorDecoder)
import Login.Messages exposing (Msg(..))
import Login.Types exposing (Field(..), Model)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Room.Messages as Room
import Util exposing (authLocation, message)


init : Maybe String -> Model
init nextUrl =
    { username = initFormField
    , password = initFormField
    , error = ""
    , submitting = False
    , nextUrl = Maybe.withDefault "/play" nextUrl
    }


update : Model -> Msg -> Flags -> ( Model, Cmd Main.Msg )
update model msg flags =
    case msg of
        Input Username username ->
            ( { model | username = updateFormField username model.username }, Cmd.none )

        Input Password password ->
            ( { model | password = updateFormField password model.password }, Cmd.none )

        Submit ->
            ( { model | submitting = True, error = "" }
            , Http.post
                { url = authLocation flags ++ "/login"
                , body =
                    Http.multipartBody
                        [ Http.stringPart "username" model.username.value
                        , Http.stringPart "password" model.password.value
                        ]
                , expect =
                    Http.expectJson
                        (Main.RoomMsg << Room.LoginMsg << SubmitCallback)
                        (maybe loginErrorDecoder)
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
                        401 ->
                            ( { model
                                | error = "Incorrect username/password"
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
                        | error = "Could not connect to authentication service"
                        , submitting = False
                      }
                    , Cmd.none
                    )

        SetNextUrl url ->
            ( { model | nextUrl = url }
            , Cmd.none
            )


keyPress : Key -> Cmd Main.Msg
keyPress code =
    case code of
        EnterKey ->
            message <| Main.RoomMsg <| Room.LoginMsg <| Submit


receive : String -> Cmd Main.Msg
receive _ =
    Cmd.none


usernameValidator : Validator Model Field
usernameValidator { username } =
    if String.length username.value == 0 then
        [ { field = Username
          , error = Error "Required"
          , touched = username.touched
          }
        ]

    else
        []


passwordValidator : Validator Model Field
passwordValidator { password } =
    if String.length password.value == 0 then
        [ { field = Password
          , error = Error "Required"
          , touched = password.touched
          }
        ]

    else
        []


validator : Model -> List (ValidationResult Field)
validator =
    batchValidators [ usernameValidator, passwordValidator ]
