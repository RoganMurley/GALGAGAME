module Login.View exposing (logoutView, view)

import Form exposing (FormFieldClass, FormFieldType(..), ValidationResult, formInputView)
import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (autofocus, class, disabled)
import Html.Events exposing (onClick)
import Login.Messages exposing (Msg(..))
import Login.State exposing (validator)
import Login.Types exposing (Field(..), Model)
import Main.Messages as Main
import Main.Types exposing (Flags)


view : Model -> Html Msg
view model =
    let
        validations : List (ValidationResult Field)
        validations =
            validator model

        submitDisabled : Bool
        submitDisabled =
            List.length validations > 0 || model.submitting

        formFieldClass : FormFieldClass Field Msg
        formFieldClass =
            { getFieldPlaceholder = getFieldPlaceholder
            , getFieldLabel = \_ -> Nothing
            , getFormFieldType = getFormFieldType
            , getExtraAttrs = getExtraAttrs
            , getInputMsg = Input
            , getIdentifier = getIdentifier
            }

        inputView : Field -> Html Msg
        inputView =
            formInputView formFieldClass validations
    in
    div []
        [ div [ class "login-box" ]
            [ inputView Username
            , inputView Password
            , button
                [ onClick Submit
                , disabled submitDisabled
                ]
                [ text "LOGIN" ]
            , div [ class "error" ] [ text model.error ]
            ]
        ]


logoutView : Flags -> List (Html Main.Msg)
logoutView { username } =
    case username of
        Just _ ->
            [ button
                [ class "settings-button", onClick Main.Logout ]
                [ text "LOGOUT" ]
            ]

        Nothing ->
            []


getFieldPlaceholder : Field -> String
getFieldPlaceholder field =
    case field of
        Username ->
            "Username"

        Password ->
            "Password"


getFormFieldType : Field -> FormFieldType
getFormFieldType field =
    case field of
        Username ->
            TextType

        Password ->
            PasswordType


getExtraAttrs : Field -> List (Attribute Msg)
getExtraAttrs field =
    case field of
        Username ->
            [ autofocus True ]

        Password ->
            []


getIdentifier : Field -> String
getIdentifier field =
    case field of
        Username ->
            "username"

        Password ->
            "password"
