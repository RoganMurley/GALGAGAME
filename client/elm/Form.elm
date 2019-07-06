module Form exposing (Error(..), FormField, FormFieldClass, FormFieldType(..), ValidationResult, Validator, batchValidators, formInputView, initFormField, updateFormField)

import Html exposing (Attribute, Html, div, input, label, text, textarea)
import Html.Attributes exposing (attribute, class, for, id, placeholder, type_)
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


type FormFieldType
    = TextType
    | EmailType
    | CheckboxType
    | PasswordType
    | TextAreaType


type alias FormFieldClass field msg =
    { getFieldPlaceholder : field -> String
    , getFieldLabel : field -> Maybe String
    , getFormFieldType : field -> FormFieldType
    , getExtraAttrs : field -> List (Attribute msg)
    , getInputMsg : field -> (String -> msg)
    , getIdentifier : field -> String
    }


batchValidators : List (Validator model field) -> Validator model field
batchValidators validators =
    \model -> List.concat <| List.map ((|>) model) validators


inputType : FormFieldType -> Attribute msg
inputType fieldType =
    case fieldType of
        TextType ->
            type_ "text"

        EmailType ->
            type_ "email"

        CheckboxType ->
            type_ "checkbox"

        PasswordType ->
            type_ "password"

        TextAreaType ->
            attribute "data-useless" ""


formInputView : FormFieldClass field msg -> List (ValidationResult field) -> field -> Html msg
formInputView formFieldClass validations field =
    let
        { getExtraAttrs, getFormFieldType, getFieldLabel, getFieldPlaceholder, getInputMsg, getIdentifier } =
            formFieldClass

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

        fieldPlaceholder : String
        fieldPlaceholder =
            getFieldPlaceholder field

        fieldLabel : Html msg
        fieldLabel =
            case getFieldLabel field of
                Just myLabel ->
                    label [ for identifier ] [ text myLabel ]

                Nothing ->
                    text ""

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

        formFieldType : FormFieldType
        formFieldType =
            getFormFieldType field

        attrs : List (Attribute msg)
        attrs =
            [ id identifier, onInput <| getInputMsg field, placeholder fieldPlaceholder ]
                ++ errorClass
                ++ extraAttrs
                ++ [ inputType formFieldType ]

        identifier : String
        identifier =
            getIdentifier field

        element : List (Attribute msg) -> List (Html msg) -> Html msg
        element =
            case formFieldType of
                TextAreaType ->
                    textarea

                _ ->
                    input
    in
    div [ class "form-input" ]
        [ element attrs []
        , fieldLabel
        , div [ class "form-error-message" ] [ text errorMessage ]
        ]
