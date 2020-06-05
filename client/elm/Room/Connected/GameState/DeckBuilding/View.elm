module DeckBuilding.View exposing (view)

import DeckBuilding.Messages exposing (Msg(..))
import DeckBuilding.Types exposing (Character, Model)
import Game.State exposing (bareContextInit)
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class, height, src, width)
import Html.Events exposing (onClick)
import Render.Types as Render
import RuneSelect.Types as RuneSelect exposing (RuneCursor(..))
import RuneSelect.View as RuneSelect
import Texture.State as Texture
import Texture.Types as Texture
import WebGL
import WebGL.Texture as WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Render.Params -> Model -> Texture.Model -> Html Msg
view { w, h, pixelRatio } model textures =
    let
        ctx =
            bareContextInit ( w, h ) textures
    in
    div [ class "character-select" ]
        [ WebGL.toHtml
            [ width <| floor <| toFloat w * pixelRatio, height <| floor <| toFloat h * pixelRatio, class "webgl-canvas" ]
          <|
            List.concat <|
                List.map ((|>) ctx)
                    (case model.runeSelect of
                        Just runeSelect ->
                            [ RuneSelect.webglView runeSelect ]

                        Nothing ->
                            []
                    )
        , case model.runeSelect of
            Nothing ->
                div []
                    [ h1 [] [ text "CHARACTER SELECT" ]
                    , charactersView model
                    ]

            Just runeSelect ->
                div [ class "rune-select" ]
                    [ h1 [] [ text "BREWING" ]
                    , RuneSelect.view runeSelect
                    ]
        ]


charactersView : Model -> Html Msg
charactersView { characters } =
    div [ class "characters" ] <|
        [ spacer
        , prevButton
        , characterView characters.selected
        , nextButton
        , spacer
        ]


prevButton : Html Msg
prevButton =
    div [ class "prev-button", onClick PreviousCharacter ] []


nextButton : Html Msg
nextButton =
    div [ class "next-button", onClick NextCharacter ] []


spacer : Html msg
spacer =
    div [ class "spacer" ] []


characterView : Character -> Html Msg
characterView ({ runeA, runeB, runeC } as character) =
    div [ class "character" ]
        [ spacer
        , div
            [ class "character-circle" ]
            [ div [ class "name" ] [ text character.name ]
            , div [ class "character-runes" ]
                [ img [ class "character-rune", src <| "/img/textures/" ++ runeA.imgURL, onClick <| EnterRuneSelect RuneCursorA ] []
                , img [ class "character-rune", src <| "/img/textures/" ++ runeB.imgURL, onClick <| EnterRuneSelect RuneCursorB ] []
                , img [ class "character-rune", src <| "/img/textures/" ++ runeC.imgURL, onClick <| EnterRuneSelect RuneCursorC ] []
                ]
            ]
        , button [ class "menu-button", class "ready-button", onClick <| Select character ] [ text "SELECT" ]
        , spacer
        ]
