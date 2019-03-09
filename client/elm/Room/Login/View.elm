module Login.View exposing (logoutView, view)

import Form exposing (Error)
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (autofocus, class, disabled, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Login.Messages exposing (Msg(..))
import Login.State exposing (validator)
import Login.Types exposing (Field(..), Model)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Maybe.Extra as Maybe


view : Model -> Html Msg
view model =
    let
        validation : Maybe ( Field, Error )
        validation =
            validator model

        submitDisabled : Bool
        submitDisabled =
            Maybe.isJust validation || model.submitting
    in
    div []
        [ div [ class "login-box" ]
            [ inputView Username validation
            , inputView Password validation
            , button
                [ onClick Submit
                , disabled submitDisabled
                ]
                [ text "Login" ]
            , div [ class "error" ] [ text model.error ]
            ]
        ]


logoutView : Flags -> List (Html Main.Msg)
logoutView { username } =
    case username of
        Just _ ->
            [ button
                [ class "settings-button", onClick Main.Logout ]
                [ text "Logout" ]
            ]

        Nothing ->
            []


inputView : Field -> Maybe ( Field, Error ) -> Html Msg
inputView field validation =
    let
        fieldLabel : String
        fieldLabel =
            case field of
                Username ->
                    "Username"

                Password ->
                    "Password"

        extraAttrs : List (Attribute Msg)
        extraAttrs =
            case field of
                Username ->
                    [ autofocus True ]

                Password ->
                    [ type_ "password" ]

        errorClass : List (Attribute Msg)
        errorClass =
            case validation of
                Just ( errField, _ ) ->
                    if field == errField then
                        [ class "login-error" ]

                    else
                        []

                Nothing ->
                    []

        attrs : List (Attribute Msg)
        attrs =
            [ onInput <| Input field, placeholder fieldLabel ] ++ errorClass ++ extraAttrs
    in
    input attrs []
