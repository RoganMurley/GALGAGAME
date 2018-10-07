module Login.View exposing (logoutView, view)

import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, disabled, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Login.Messages exposing (Input(..), Msg(..))
import Login.Types exposing (Model)
import Login.State exposing (passwordInvalid, usernameInvalid)
import Main.Messages as Main
import Main.Types exposing (Flags)


view : Model -> Html Msg
view { username, password, error, submitting } =
    let
        submitDisabled : Bool
        submitDisabled =
            usernameInvalid username
                || passwordInvalid password
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
