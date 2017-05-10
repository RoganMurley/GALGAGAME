module CharacterSelect.View exposing (..)

import Card exposing (Card)
import CharacterSelect.Types exposing (Character, Model)
import CharacterSelect.Messages as CharacterSelect
import GameState.Messages as GameState
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Messages exposing (Msg(..))
import Raymarch.Types as Raymarch
import Raymarch.View as Raymarch


view : Raymarch.Params -> Model -> Html Msg
view (Raymarch.Params frameTime windowDimensions) { characters, selected, hover } =
    let
        characterView : Character -> Html Msg
        characterView ({ name, imgURL } as character) =
            div
                [ class "character-button"
                , onMouseEnter
                    (GameStateMsg
                        (GameState.SelectingMsg
                            (CharacterSelect.Hover name)
                        )
                    )
                , onClick (SelectCharacter name)
                , if (List.member character selected) then
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
                chosenView { name, imgURL } =
                    div
                        [ class "character-chosen"
                        , onMouseEnter
                            (GameStateMsg
                                (GameState.SelectingMsg
                                    (CharacterSelect.Hover name)
                                )
                            )
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
                        div [ class "ready-up" ] [ text "Waiting for opponent" ]
                      else
                        div [] []
                    ]

        cardPreviewView : ( Card, Card, Card, Card ) -> Html Msg
        cardPreviewView ( c1, c2, c3, c4 ) =
            div
                [ class "card-preview" ]
                [ Card.view c1
                , Card.view c2
                , Card.view c3
                , Card.view c4
                ]
    in
        div []
            [ div
                [ class "character-select" ]
                [ text "Choose your Gods"
                , div [ class "characters" ]
                    (List.map characterView characters)
                , cardPreviewView ((\{ cards } -> cards) hover)
                , selectedView selected
                ]
            , div [] [ Raymarch.view (Raymarch.Params frameTime windowDimensions) ]
            ]
