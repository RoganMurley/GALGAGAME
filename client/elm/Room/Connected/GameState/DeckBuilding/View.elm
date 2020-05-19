module DeckBuilding.View exposing (view)

import DeckBuilding.Messages exposing (Msg(..))
import DeckBuilding.Types exposing (Character, Model, Rune, RuneSelectModel)
import Game.State exposing (bareContextInit)
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class, height, src, width)
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
        , case model.runeSelect of
            Nothing ->
                div []
                    [ h1 [] [ text "HEIR SELECT" ]
                    , charactersView model
                    ]

            Just runeSelect ->
                div []
                    [ h1 [] [ text "RUNES" ]
                    , runesView runeSelect model.runes
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
    div
        [ class "character" ]
        [ div [ class "name" ] [ text character.name ]

        -- , div [ class "portrait" ] [ img [ src character.imgUrl ] [] ]
        , div [ class "character-runes" ]
            [ img [ class "character-rune", src <| "/img/textures/" ++ runeA.imgURL, onClick <| EnterRuneSelect runeA runeB runeC 0 ] []
            , img [ class "character-rune", src <| "/img/textures/" ++ runeB.imgURL, onClick <| EnterRuneSelect runeB runeA runeC 1 ] []
            , img [ class "character-rune", src <| "/img/textures/" ++ runeC.imgURL, onClick <| EnterRuneSelect runeC runeA runeB 2 ] []
            ]
        , button [ class "ready-button", onClick <| Select character ] [ text "SELECT" ]
        ]



-- Rune view


runesView : RuneSelectModel -> List Rune -> Html Msg
runesView { excluded1, excluded2, index } allRunes =
    let
        runes : List Rune
        runes =
            List.filter
                (\rune -> (rune /= excluded1) && (rune /= excluded2))
                allRunes

        runeView : Rune -> Html Msg
        runeView rune =
            img [ class "rune", src <| "/img/textures/" ++ rune.imgURL, onClick <| UpdateRune rune index ] []
    in
    div [ class "runes" ] <|
        List.concat
            [ [ spacer ]
            , List.map runeView runes
            , [ spacer ]
            ]
