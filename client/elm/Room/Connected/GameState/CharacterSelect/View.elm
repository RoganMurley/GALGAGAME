module CharacterSelect.View exposing (..)

import Card.Types exposing (Card)
import CharacterSelect.Messages exposing (Msg(..))
import CharacterSelect.Types exposing (Character, Model)
import Html exposing (Html, div, img, text, table, td, th, tr)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick, onMouseEnter)


view : Model -> Html Msg
view { characters, selected, vm } =
    let
        characterView : Character -> Html Msg
        characterView character =
            div
                [ class "character-button"
                , onMouseEnter <| Hover character
                , onClick <| Select character
                , if List.member character selected then
                    class "invisible"
                  else
                    class ""
                ]
                [ img [ src ("/img/" ++ character.imgURL), class "character-icon" ] []
                ]

        selectedView : Html Msg
        selectedView =
            let
                chosenView : Character -> Html Msg
                chosenView character =
                    div
                        [ class "character-chosen"
                        , onMouseEnter <| Hover character
                        ]
                        [ img [ src ("/img/" ++ character.imgURL), class "character-icon" ] [] ]

                unchosen : List (Html Msg)
                unchosen =
                    List.repeat
                        (3 - List.length selected)
                        (div [ class "character-unchosen" ] [])
            in
                div []
                    [ div
                        [ class "characters-all-chosen" ]
                        (List.map chosenView selected ++ unchosen)
                    , div
                        [ class "ready-up" ]
                        [ if List.length selected >= 3 then
                            text "Waiting for opponent"
                          else
                            text ""
                        ]
                    ]

        cardPreviewView : ( Card, Card, Card, Card ) -> Html Msg
        cardPreviewView ( c1, c2, c3, c4 ) =
            let
                eachView : Card -> Html Msg
                eachView c =
                    td [] [ cardView c ]
            in
                table [ class "card-preview" ]
                    [ tr [] (List.map eachView [ c1, c2, c3, c4 ]) ]
    in
        div []
            [ div
                [ class "character-select" ]
                [ text "Choose your Characters"
                , div [ class "characters" ] <| List.map characterView characters
                , cardPreviewView (.cards vm.hover)
                , selectedView
                ]
            ]


cardView : Card -> Html msg
cardView { name, desc, imgURL } =
    div
        [ class "card"
        ]
        [ div [ class "card-title" ] [ text name ]
        , div
            [ class "card-picture"
            , style [ ( "background-image", "url(\"/img/" ++ imgURL ++ "\")" ) ]
            ]
            []
        , div [ class "card-desc" ] [ text desc ]
        ]
