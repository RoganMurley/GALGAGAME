module Signup.View exposing (view)

import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (autofocus, class, disabled, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Signup.Messages exposing (Input(..), Msg(..))
import Signup.State exposing (submitDisabled)
import Signup.Types exposing (Model)


view : Model -> Html Msg
view model =
    div []
        [ div [ class "login-box" ]
            [ text "Username"
            , input
                [ placeholder "Username"
                , autofocus True
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
                , disabled <| submitDisabled model
                ]
                [ text "Signup" ]
            , div [ class "error" ] [ text model.error ]
            ]
        ]
