module Login.View exposing (logoutView, view)

import Form exposing (Error)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (autofocus, class, disabled, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Login.Messages exposing (Input(..), Msg(..))
import Login.State exposing (validator)
import Login.Types exposing (Field, Model)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Maybe.Extra as Maybe


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
            [ div [ class "login-label" ] [ text "Username" ]
            , input
                [ placeholder "Username"
                , autofocus True
                , onInput <| Input Username
                ]
                []
            , div [ class "login-label" ] [ text "Password" ]
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
