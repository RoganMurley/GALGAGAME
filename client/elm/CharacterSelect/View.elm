module CharacterSelect.View exposing (..)

import Card.Types exposing (Card)
import Card.View as Card
import CharacterSelect.Messages exposing (..)
import CharacterSelect.Types exposing (Character, Model)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Raymarch.Types as Raymarch
import Raymarch.View as Raymarch


view : Raymarch.Params -> Model -> Html Msg
view params { characters, selected, hover } =
    let
        characterView : Character -> Html Msg
        characterView ({ name, imgURL } as character) =
            div
                [ class "character-button"
                , onMouseEnter <| Hover character
                , onClick <| Select character
                , if List.member character selected then
                    class "invisible"
                  else
                    class ""
                ]
                [ img [ src ("img/" ++ imgURL), class "character-icon" ] []
                , div [ class "character-name" ] [ text name ]
                ]

        selectedView : List Character -> Html Msg
        selectedView selected =
            let
                chosenView : Character -> Html Msg
                chosenView ({ name, imgURL } as character) =
                    div
                        [ class "character-chosen"
                        , onMouseEnter <| Hover character
                        ]
                        [ img [ src ("img/" ++ imgURL), class "character-icon" ] []
                        , div [ class "character-name" ] [ text name ]
                        ]
            in
                div []
                    [ div
                        [ class "characters-all-chosen" ]
                        (List.map chosenView selected)
                    , if List.length selected >= 3 then
                        div
                            [ class "ready-up" ]
                            [ text "Waiting for opponent" ]
                      else
                        div [] []
                    ]

        cardPreviewView : ( Card, Card, Card, Card ) -> Html Msg
        cardPreviewView ( c1, c2, c3, c4 ) =
            div
                [ class "card-preview" ]
                (List.map Card.view [ c1, c2, c3, c4 ])
    in
        div []
            [ div
                [ class "character-select" ]
                [ text "Choose your Classes"
                , div [ class "characters" ]
                    (List.map characterView characters)
                , cardPreviewView ((\{ cards } -> cards) hover)
                , selectedView selected
                ]
            , div [] [ Raymarch.view params ]
            ]
