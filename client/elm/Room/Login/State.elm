module Login.State exposing (init, keyPress, receive, update, validator)

import Form exposing (Error(..))
import Http
import Json.Decode exposing (maybe)
import Keyboard exposing (KeyCode)
import Login.Decoders exposing (loginErrorDecoder)
import Login.Messages exposing (Msg(..))
import Login.Types exposing (Field(..), FormField, Model, ValidationResult)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Navigation
import Room.Messages as Room
import Util exposing (authLocation, message, send)


formFieldInit : FormField
formFieldInit =
    { value = "", touched = False }


updateField : String -> FormField -> FormField
updateField newValue field =
    { field | value = newValue, touched = True }


init : Maybe String -> Model
init nextUrl =
    { username = formFieldInit
    , password = formFieldInit
    , error = ""
    , submitting = False
    , nextUrl = Maybe.withDefault "/" nextUrl
    }


update : Model -> Msg -> Flags -> ( Model, Cmd Main.Msg )
update model msg flags =
    case msg of
        Input Username username ->
            ( { model | username = updateField username model.username }, Cmd.none )

        Input Password password ->
            ( { model | password = updateField password model.password }, Cmd.none )

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


usernameValidator : { a | username : FormField } -> Maybe ValidationResult
usernameValidator { username } =
    if String.length username.value == 0 then
        Just { field = Username, error = Error "Enter a password", touched = username.touched }

    else
        Nothing


passwordValidator : { a | password : FormField } -> Maybe ValidationResult
passwordValidator { password } =
    if String.length password.value == 0 then
        Just { field = Password, error = Error "Enter a password", touched = password.touched }

    else
        Nothing


validator : Model -> List ValidationResult
validator model =
    List.filterMap identity <|
        List.map ((|>) model) [ passwordValidator, usernameValidator ]
