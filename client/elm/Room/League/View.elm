module League.View exposing (view)

import Form exposing (Error(..), FormFieldType(..))
import Html exposing (Html, a, button, div, h1, p, text)
import Html.Attributes exposing (class, disabled, href)
import Html.Events exposing (onClick)
import League.Messages exposing (Msg(..))
import League.Types exposing (Model, SubmitState(..))


view : Model -> Html Msg
view model =
    div []
        [ div [ class "league-box" ]
            (case model.submitState of
                Waiting ->
                    []

                Submitted ->
                    [ h1 [] [ text "GALGA LEAGUE" ]
                    , p []
                        [ text "You have joined the league! We'll be in touch :)" ]
                    , a [ href "/play/quickplay" ] [ text "In the meantime, go play!" ]
                    ]

                _ ->
                    [ h1 [] [ text "GALGA LEAGUE" ]
                    , p [] [ text "Prove your mastery of the wheel." ]
                    , p [] [ text "You'll be matched with other players each week for six weeks." ]
                    , p [] [ text "Play best of three matches at times that suit you." ]
                    , p [] [ text "Glory and prizes await!" ]
                    , button
                        [ onClick Submit
                        , disabled <| model.submitState == Submitting
                        , class "menu-button"
                        ]
                        [ text "JOIN" ]
                    , div [ class "error" ] [ text model.error ]
                    ]
            )
        ]
