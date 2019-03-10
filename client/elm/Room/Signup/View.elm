module Signup.View exposing (view)

import Form exposing (Error(..), FormFieldClass, ValidationResult, formInputView)
import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (autofocus, class, disabled, type_)
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
            { getFieldLabel = getFieldLabel
            , getExtraAttrs = getExtraAttrs
            , getInputMsg = Input
            }

        inputView : Field -> Html Msg
        inputView =
            formInputView formFieldClass validations
    in
    div []
        [ div [ class "login-box" ]
            [ inputView Email
            , inputView Username
            , inputView Password
            , button
                [ onClick Submit
                , disabled submitDisabled
                ]
                [ text "Signup" ]
            , div [ class "error" ] [ text model.error ]
            ]
        ]


getFieldLabel : Field -> String
getFieldLabel field =
    case field of
        Email ->
            "Email"

        Username ->
            "Username"

        Password ->
            "Password"


getExtraAttrs : Field -> List (Attribute Msg)
getExtraAttrs field =
    case field of
        Email ->
            [ autofocus True, type_ "email" ]

        Username ->
            []

        Password ->
            [ type_ "password" ]
