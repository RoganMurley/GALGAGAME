module Signup.View exposing (view)

import Form exposing (Error(..))
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (autofocus, class, disabled, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra as Maybe
import Signup.Messages exposing (Msg(..))
import Signup.State exposing (validator)
import Signup.Types exposing (Field(..), Model)


view : Model -> Html Msg
view model =
    let
        validation : Maybe ( Field, Error )
        validation =
            validator model

        submitDisabled : Bool
        submitDisabled =
            Maybe.isJust validation || model.submitting
    in
    div []
        [ div [ class "login-box" ]
            [ inputView Email validation
            , inputView Username validation
            , inputView Password validation
            , button
                [ onClick Submit
                , disabled submitDisabled
                ]
                [ text "Signup" ]
            , div [ class "error" ] [ text model.error ]
            ]
        ]


inputView : Field -> Maybe ( Field, Error ) -> Html Msg
inputView field validation =
    let
        fieldLabel : String
        fieldLabel =
            case field of
                Email ->
                    "Email"

                Username ->
                    "Username"

                Password ->
                    "Password"

        extraAttrs : List (Attribute Msg)
        extraAttrs =
            case field of
                Email ->
                    [ autofocus True, type_ "email" ]

                Username ->
                    []

                Password ->
                    [ type_ "password" ]

        errorClass : List (Attribute Msg)
        errorClass =
            case validation of
                Just ( errField, _ ) ->
                    if field == errField then
                        [ class "login-error" ]

                    else
                        []

                Nothing ->
                    []

        attrs : List (Attribute Msg)
        attrs =
            [ onInput <| Input field, placeholder fieldLabel ] ++ errorClass ++ extraAttrs
    in
    input attrs []
