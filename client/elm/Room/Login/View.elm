module Login.View exposing (logoutView, view)

import Form exposing (FormFieldClass, ValidationResult, formInputView)
import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (autofocus, class, disabled, type_)
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
            [ inputView Username
            , inputView Password
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


getFieldLabel : Field -> String
getFieldLabel field =
    case field of
        Username ->
            "Username"

        Password ->
            "Password"


getExtraAttrs : Field -> List (Attribute Msg)
getExtraAttrs field =
    case field of
        Username ->
            [ autofocus True ]

        Password ->
            [ type_ "password" ]



-- inputView : Field -> List (ValidationResult Field) -> Html Msg
-- inputView field validations =
--     let
--         validation : Maybe (ValidationResult Field)
--         validation =
--             List.find (.field >> (==) field) validations
--
--         error : Maybe Error
--         error =
--             Maybe.map .error validation
--
--         touched : Bool
--         touched =
--             Maybe.withDefault True <|
--                 Maybe.map .touched validation
--
--         fieldLabel : String
--         fieldLabel =
--             case field of
--                 Username ->
--                     "Username"
--
--                 Password ->
--                     "Password"
--
--         extraAttrs : List (Attribute Msg)
--         extraAttrs =
--             case field of
--                 Username ->
--                     [ autofocus True ]
--
--                 Password ->
--                     [ type_ "password" ]
--
--         errorClass : List (Attribute Msg)
--         errorClass =
--             if Maybe.isJust error && touched then
--                 [ class "login-error" ]
--
--             else
--                 []
--
--         attrs : List (Attribute Msg)
--         attrs =
--             [ onInput <| Input field, placeholder fieldLabel ]
--                 ++ errorClass
--                 ++ extraAttrs
--     in
--     input attrs []
