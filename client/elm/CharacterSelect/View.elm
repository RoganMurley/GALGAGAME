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
                  -- , div [ class "character-name" ] [ text name ]
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
                          -- , div [ class "character-name" ] [ text name ]
                        ]

                unchosen : List (Html Msg)
                unchosen =
                    List.repeat
                        (3 - (List.length selected))
                        (div [ class "character-unchosen" ] [])
            in
                div []
                    [ div
                        [ class "characters-all-chosen" ]
                        ((List.map chosenView selected) ++ unchosen)
                    , if List.length selected >= 3 then
                        div
                            [ class "ready-up" ]
                            [ text "Waiting for opponent" ]
                      else
                        div [] []
                    ]

        cardPreviewView : ( Card, Card, Card, Card ) -> Html Msg
        cardPreviewView ( c1, c2, c3, c4 ) =
            let
                eachView : Card -> Html Msg
                eachView c =
                    td [] [ Card.view c ]
            in
                table [ class "card-preview" ]
                    [ tr [] (List.map eachView [ c1, c2, c3, c4 ])
                    , tr []
                        (List.map
                            (\x -> th [ class "card-type" ] [ text x ])
                            [ "Weapon", "Magic", "Utility", "Control" ]
                        )
                    ]
    in
        div []
            [ div
                [ class "character-select" ]
                [ text "Choose your Weapons"
                , div [ class "characters" ]
                    (List.map characterView characters)
                , cardPreviewView ((\{ cards } -> cards) hover)
                , selectedView selected
                ]
            , div [] [ Raymarch.view params ]
            ]
