module Create.View exposing (view)

import Create.Messages exposing (Msg(..))
import Create.State exposing (validator)
import Create.Types exposing (Field(..), Model)
import Form exposing (FormFieldClass, FormFieldType(..), ValidationResult, formInputView)
import Html exposing (Attribute, Html, div, h1, text)
import Html.Attributes exposing (class)
import Main.Messages as Main


view : Model -> Html Msg
view model =
    let
        validations : List (ValidationResult Field)
        validations =
            validator model

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
    div [ class "create-box" ]
        [ div [ class "create" ]
            [ h1 [] [ text "CREATE CUSTOM GAME" ]
            , inputView StartingLife
            , div [ class "error" ] [ text model.error ]
            ]
        ]


getFieldPlaceholder : Field -> String
getFieldPlaceholder field =
    case field of
        StartingLife ->
            "Starting Life"


getFormFieldType : Field -> FormFieldType
getFormFieldType field =
    case field of
        StartingLife ->
            TextType


getExtraAttrs : Field -> List (Attribute Msg)
getExtraAttrs field =
    case field of
        StartingLife ->
            []


getIdentifier : Field -> String
getIdentifier field =
    case field of
        StartingLife ->
            "starting-life"
