module Signup.View exposing (view)

import Form exposing (Error(..), FormFieldClass, FormFieldType(..), ValidationResult, formInputView)
import Html exposing (Attribute, Html, button, div, h1, text)
import Html.Attributes exposing (autofocus, class, disabled)
import Html.Events exposing (onClick)
import Signup.Messages exposing (Msg(..))
import Signup.State exposing (validator)
import Signup.Types exposing (Field(..), Model)


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
            , getFieldLabel = getFieldLabel
            , getFormFieldType = getFormFieldType
            , getExtraAttrs = getExtraAttrs
            , getInputMsg = Input
            , getIdentifier = getIdentifier
            }

        inputView : Field -> Html Msg
        inputView =
            formInputView formFieldClass validations
    in
    div [ class "signup-box" ]
        [ div [ class "signup" ]
            [ h1 [] [ text "SIGN UP FOR GALGA" ]
            , inputView Email
            , inputView Username
            , inputView Password
            , inputView Contactable
            , button
                [ onClick Submit
                , disabled submitDisabled
                , class "button"
                ]
                [ text "SIGN UP" ]
            , div [ class "error" ] [ text model.error ]
            ]
        ]


getFieldPlaceholder : Field -> String
getFieldPlaceholder field =
    case field of
        Email ->
            "Email"

        Username ->
            "Username"

        Password ->
            "Password"

        Contactable ->
            ""


getFieldLabel : Field -> Maybe String
getFieldLabel field =
    case field of
        Contactable ->
            Just "Let us contact you with sweet, sweet updates"

        _ ->
            Nothing


getFormFieldType : Field -> FormFieldType
getFormFieldType field =
    case field of
        Email ->
            EmailType

        Username ->
            TextType

        Password ->
            PasswordType

        Contactable ->
            CheckboxType


getExtraAttrs : Field -> List (Attribute Msg)
getExtraAttrs field =
    case field of
        Email ->
            [ autofocus True ]

        _ ->
            []


getIdentifier : Field -> String
getIdentifier field =
    case field of
        Email ->
            "email"

        Username ->
            "username"

        Password ->
            "password"

        Contactable ->
            "contactable"
