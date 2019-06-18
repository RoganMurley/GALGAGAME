module Login.State exposing (init, keyPress, receive, update, validator)

import Form exposing (Error(..), ValidationResult, Validator, batchValidators, initFormField, updateFormField)
import Http
import Json.Decode exposing (maybe)
import Keyboard exposing (KeyCode)
import Login.Decoders exposing (loginErrorDecoder)
import Login.Messages exposing (Msg(..))
import Login.Types exposing (Field(..), Model)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Navigation
import Ports exposing (websocketSend)
import Room.Messages as Room
import Util exposing (authLocation, message)


init : Maybe String -> Model
init nextUrl =
    { username = initFormField
    , password = initFormField
    , error = ""
    , submitting = False
    , nextUrl = Maybe.withDefault "/" nextUrl
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
            , Http.send
                (Main.RoomMsg << Room.LoginMsg << SubmitCallback)
              <|
                Http.post
                    (authLocation flags ++ "/login")
                    (Http.multipartBody
                        [ Http.stringPart "username" model.username.value
                        , Http.stringPart "password" model.password.value
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
                  , -- Reconnect so that the ws connection has our login cookie
                    websocketSend "reconnect:"
                  ]

        SubmitCallback (Err httpError) ->
            case httpError of
                Http.BadStatus { status } ->
                    case status.code of
                        401 ->
                            ( { model
                                | error = "Incorrect username/password"
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
                        | error = "Could not connect to authentication service"
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
