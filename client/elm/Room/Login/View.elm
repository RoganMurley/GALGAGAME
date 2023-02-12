module Login.View exposing (loginoutView, view)

import Form exposing (FormFieldClass, FormFieldType(..), ValidationResult, formInputView)
import Html exposing (Attribute, Html, a, button, div, h1, text)
import Html.Attributes exposing (autofocus, class, disabled, href, id, target)
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
    div [ class "login-box" ]
        [ div [ class "login" ]
            [ h1 [] [ text "LOG IN TO GALGA" ]
            , inputView Username
            , inputView Password
            , button
                [ onClick Submit
                , disabled submitDisabled
                , class "button"
                ]
                [ text "LOG IN" ]
            , a [ class "signup-link", href "/signup" ] [ text "No account? Signup!" ]
            , div [ class "error" ] [ text model.error ]
            ]
        ]


loginoutView : Flags -> List (Html Main.Msg)
loginoutView { username } =
    case username of
        Just u ->
            [ a
                [ class "settings-button"
                , class "profile-link"
                , target "_blank"
                , href <| "/profile/" ++ u
                ]
                [ text "VIEW PROFILE" ]
            , button
                [ class "settings-button"
                , target "#"
                , id "fullscreen-button"
                ]
                [ text "TOGGLE FULLSCREEN" ]
            , button
                [ class "settings-button"
                , onClick Main.Logout
                ]
                [ text "LOGOUT" ]
            ]

        Nothing ->
            [ button
                [ class "settings-button"
                , onClick Main.GotoSignup
                ]
                [ text "SIGNUP" ]
            , button
                [ class "settings-button"
                , onClick Main.GotoLogin
                ]
                [ text "LOGIN" ]
            ]


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
