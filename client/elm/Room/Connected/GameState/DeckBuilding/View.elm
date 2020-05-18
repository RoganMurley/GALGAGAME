module DeckBuilding.View exposing (view)

import DeckBuilding.Messages exposing (Msg(..))
import DeckBuilding.Types exposing (Character, Model)
import Game.State exposing (bareContextInit)
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class, classList, height, src, width)
import Html.Events exposing (onClick)
import Render.Types as Render
import Texture.State as Texture
import Texture.Types as Texture
import WebGL
import WebGL.Texture as WebGL


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
                    []
        , h1 [] [ text "HEIR SELECT" ]
        , charactersView model
        ]


charactersView : Model -> Html Msg
charactersView { characters } =
    div [ class "characters" ] <|
        [ prevButton
        , characterView characters.selected
        , nextButton
        ]


prevButton : Html Msg
prevButton =
    div [ class "prev-button", onClick PreviousCharacter ] []


nextButton : Html Msg
nextButton =
    div [ class "next-button", onClick NextCharacter ] []


characterView : Character -> Html Msg
characterView ({ runeA, runeB, runeC } as character) =
    div
        [ classList [ ( "character", True ) ] ]
        [ div [ class "name" ] [ text character.name ]

        -- , div [ class "portrait" ] [ img [ src character.imgUrl ] [] ]
        , div [ class "runes" ]
            [ img [ class "rune", src <| "/img/textures/" ++ runeA.imgURL ] []
            , img [ class "rune", src <| "/img/textures/" ++ runeB.imgURL ] []
            , img [ class "rune", src <| "/img/textures/" ++ runeC.imgURL ] []
            ]
        , button [ class "ready-button", onClick <| Select character ] [ text "SELECT" ]
        ]
