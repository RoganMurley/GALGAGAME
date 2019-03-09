module Signup.View exposing (view)

import Error exposing (Error(..))
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (autofocus, class, disabled, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra as Maybe
import Signup.Messages exposing (Input(..), Msg(..))
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
            [ text "Email"
            , input
                [ placeholder "Email"
                , autofocus True
                , onInput <| Input Email
                , type_ "email"
                ]
                []
            , text
                "Username"
            , input
                [ placeholder "Username"
                , onInput <| Input Username
                ]
                []
            , text "Password"
            , input
                [ placeholder "Password"
                , onInput <| Input Password
                , type_ "password"
                ]
                []
            , text "Confirm Password"
            , input
                [ placeholder "Confirm Password"
                , onInput <| Input ConfirmPassword
                , type_ "password"
                ]
                []
            , button
                [ onClick Submit
                , disabled submitDisabled
                ]
                [ text "Signup" ]
            , div [ class "error" ] [ text model.error ]
            ]
        ]
