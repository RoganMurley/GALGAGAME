module Create.View exposing (view)

import Create.Messages exposing (Msg(..))
import Create.State exposing (validator)
import Create.Types exposing (Field(..), Model)
import Debug exposing (toString)
import Form exposing (FormFieldClass, FormFieldType(..), ValidationResult, formInputView)
import Html exposing (Attribute, Html, button, div, h1, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)


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
    div [ class "create-box" ]
        [ div [ class "create" ]
            [ h1 [] [ text "CREATE CUSTOM GAME" ]
            , inputView AllowSpectators
            , inputView StartingLife
            , button
                [ onClick Submit
                , disabled submitDisabled
                , class "button"
                ]
                [ text "CREATE" ]
            , div [] [ text (String.fromInt (List.length validations)) ]
            , div [ class "error" ] [ text model.error ]
            ]
        ]


getFieldPlaceholder : Field -> String
getFieldPlaceholder field =
    case field of
        StartingLife ->
            "Starting Life"

        AllowSpectators ->
            ""


getFieldLabel : Field -> Maybe String
getFieldLabel field =
    case field of
        StartingLife ->
            Nothing

        AllowSpectators ->
            Just "Allow spectators"


getFormFieldType : Field -> FormFieldType
getFormFieldType field =
    case field of
        StartingLife ->
            TextType

        AllowSpectators ->
            CheckboxType


getExtraAttrs : Field -> List (Attribute Msg)
getExtraAttrs field =
    case field of
        StartingLife ->
            []

        AllowSpectators ->
            []


getIdentifier : Field -> String
getIdentifier field =
    case field of
        StartingLife ->
            "starting-life"

        AllowSpectators ->
            "allow-spectators"
