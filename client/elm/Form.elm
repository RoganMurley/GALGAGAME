module Form exposing (Error(..), FormField, FormFieldClass, ValidationResult, Validator, batchValidators, formInputView, initFormField, updateFormField)

import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onInput)
import List.Extra as List
import Maybe.Extra as Maybe


initFormField : FormField
initFormField =
    { value = "", touched = False }


updateFormField : String -> FormField -> FormField
updateFormField newValue field =
    { field | value = newValue, touched = True }


type alias FormField =
    { value : String
    , touched : Bool
    }


type alias ValidationResult field =
    { field : field
    , error : Error
    , touched : Bool
    }


type alias Validator model field =
    model -> List (ValidationResult field)


type Error
    = Error String


type alias FormFieldClass field msg =
    { getFieldLabel : field -> String
    , getExtraAttrs : field -> List (Attribute msg)
    , getInputMsg : field -> (String -> msg)
    }


batchValidators : List (Validator model field) -> Validator model field
batchValidators validators =
    \model -> List.concat <| List.map ((|>) model) validators


formInputView : FormFieldClass field msg -> List (ValidationResult field) -> field -> Html msg
formInputView { getExtraAttrs, getFieldLabel, getInputMsg } validations field =
    let
        validation : Maybe (ValidationResult field)
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
            getFieldLabel field

        extraAttrs : List (Attribute msg)
        extraAttrs =
            getExtraAttrs field

        errorClass : List (Attribute msg)
        errorClass =
            if touched then
                if Maybe.isJust error then
                    [ class "form-error" ]

                else
                    [ class "form-good" ]

            else
                []

        errorMessage : String
        errorMessage =
            case ( error, touched ) of
                ( Just (Error msg), True ) ->
                    msg

                _ ->
                    ""

        attrs : List (Attribute msg)
        attrs =
            [ onInput <| getInputMsg field, placeholder fieldLabel ]
                ++ errorClass
                ++ extraAttrs
    in
    div [ class "form-input" ]
        [ input attrs []
        , div [ class "form-error-message" ] [ text errorMessage ]
        ]
