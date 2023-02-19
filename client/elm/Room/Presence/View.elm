module Presence.View exposing (view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Presence.Types exposing (Model)


view : Model -> Html a
view model =
    div [ class "presence-box" ] <|
        case model.error of
            "" ->
                case model.presence of
                    Just usernames ->
                        List.map text usernames

                    Nothing ->
                        []

            _ ->
                [ div [ class "error" ] [ text model.error ] ]
