module Login.View exposing (logoutView, view)

import Form exposing (Error)
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (autofocus, class, disabled, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import List.Extra as List
import Login.Messages exposing (Msg(..))
import Login.State exposing (validator)
import Login.Types exposing (Field(..), Model, ValidationResult)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Maybe.Extra as Maybe


view : Model -> Html Msg
view model =
    let
        validations : List ValidationResult
        validations =
            validator model

        submitDisabled : Bool
        submitDisabled =
            List.length validations > 0 || model.submitting
    in
    div []
        [ div [ class "login-box" ]
            [ inputView Username validations
            , inputView Password validations
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


inputView : Field -> List ValidationResult -> Html Msg
inputView field validations =
    let
        validation : Maybe ValidationResult
        validation =
            List.find (.field >> (==) field) validations

        error : Maybe Error
        error =
            Maybe.map .error validation

        touched : Bool
        touched =
            Maybe.withDefault True <|
                Maybe.map .touched validation

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
            if Maybe.isJust error && touched then
                [ class "login-error" ]

            else
                []

        attrs : List (Attribute Msg)
        attrs =
            [ onInput <| Input field, placeholder fieldLabel ]
                ++ errorClass
                ++ extraAttrs
    in
    input attrs []
