module DeckBuilding.View exposing (view)

import DeckBuilding.Messages exposing (Msg(..))
import DeckBuilding.State exposing (getRuneFromCursor, nextCursor)
import DeckBuilding.Types exposing (Character, Model, Rune, RuneCursor(..), RuneSelectModel)
import Game.State exposing (bareContextInit)
import Html exposing (Html, button, div, h1, h2, img, text)
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
                    , runesView runeSelect model.characters.selected model.runes
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
        , button [ class "ready-button", onClick <| Select character ] [ text "SELECT" ]
        , spacer
        ]



-- Rune view


runesView : RuneSelectModel -> Character -> List Rune -> Html Msg
runesView { cursor, selected } character allRunes =
    let
        excluded1 : Rune
        excluded1 =
            getRuneFromCursor (nextCursor cursor) character

        excluded2 : Rune
        excluded2 =
            getRuneFromCursor ((nextCursor >> nextCursor) cursor) character

        runes : List Rune
        runes =
            List.filter
                (\rune -> (rune /= excluded1) && (rune /= excluded2))
                allRunes

        runeView : Rune -> Html Msg
        runeView rune =
            img [ class "rune", src <| "/img/textures/" ++ rune.imgURL, onClick <| SelectRune rune ] []
    in
    div [ class "runes" ]
        [ div [ class "rune-list" ] (List.map runeView runes)
        , h2 [] [ text selected.name ]
        , button [ class "rune-confirm", onClick ConfirmRune ] [ text "CHOOSE" ]
        ]
