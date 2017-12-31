module Login.View exposing (logoutView, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Login.Messages exposing (..)
import Login.Types exposing (..)
import Login.State exposing (passwordInvalid, usernameInvalid)
import Main.Messages as Main
import Raymarch.Types as Raymarch
import Raymarch.View as Raymarch


view : Raymarch.Params -> Model -> Html Msg
view params { username, password, error, submitting } =
    let
        submitDisabled : Bool
        submitDisabled =
            (usernameInvalid username)
                || (passwordInvalid password)
                || submitting
    in
        div []
            [ div [ class "login-box" ]
                [ text "Username"
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
                , button
                    [ onClick Submit
                    , disabled submitDisabled
                    ]
                    [ text "Login" ]
                , div [ class "error" ] [ text error ]
                ]
            , div
                []
                [ Raymarch.view params ]
            ]


logoutView : List (Html Main.Msg)
logoutView =
    [ button
        [ class "settings-button", onClick Main.Logout ]
        [ text "Logout" ]
    ]
