module DeckBuilding.View exposing (view)

import Colour
import DeckBuilding.Messages exposing (Msg(..))
import DeckBuilding.Types exposing (Model)
import Game.State exposing (bareContextInit)
import Game.Types exposing (Context)
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class, height, width)
import Html.Events exposing (onClick)
import Math.Matrix4 exposing (makeLookAt, makeOrtho, makeRotate, makeScale3)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Render.Shaders
import Render.Types as Render
import Render.Uniforms exposing (uniColourMag)
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
            bareContextInit ( w, h ) textures Nothing
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
                            [ webglView model ]
                    )
        , case model.runeSelect of
            Nothing ->
                div []
                    [ h1 [] [ text "WHO ARE YOU?" ]
                    , charactersView model
                    ]

            Just runeSelect ->
                div [ class "rune-select" ]
                    [ h1 [] [ text "BREWING" ]
                    , RuneSelect.view ctx runeSelect
                    ]
        ]


charactersView : Model -> Html Msg
charactersView { characters } =
    let
        character =
            characters.selected
    in
    div [ class "characters" ]
        [ div [ class "character-select-bottom" ]
            [ div [ class "character-name" ] [ text character.name ]
            , div [ class "character-last-row" ]
                [ div [ class "character-prev-button", onClick PreviousCharacter ] []
                , button [ class "character-confirm", class "menu-button", onClick <| Select character ] [ text "READY" ]
                , div [ class "character-next-button", onClick NextCharacter ] []
                ]
            ]
        ]



-- characterView : Character -> Html Msg
-- characterView ({ runeA, runeB, runeC } as character) =
--     div [ class "character" ]
--         [ spacer
--         , div
--             [ class "character-circle" ]
--             [ div [ class "name" ] [ text character.name ]
-- , div [ class "character-runes" ]
--     [ img [ class "character-rune", src <| "/img/textures/" ++ runeA.imgURL, onClick <| EnterRuneSelect RuneCursorA ] []
--     , img [ class "character-rune", src <| "/img/textures/" ++ runeB.imgURL, onClick <| EnterRuneSelect RuneCursorB ] []
--     , img [ class "character-rune", src <| "/img/textures/" ++ runeC.imgURL, onClick <| EnterRuneSelect RuneCursorC ] []
--     ]
--     ]
-- ]


webglView : Model -> Context -> List WebGL.Entity
webglView _ ({ w, h, radius, textures } as ctx) =
    List.concat
        [ [ Render.Primitives.circle <|
                uniColourMag ctx
                    Colour.tea
                    1
                    { scale = radius * 0.73
                    , position = vec2 (w * 0.5) (h * 0.5)
                    , rotation = 0
                    }
          ]
        , case Texture.load textures "tea.png" of
            Just texture ->
                [ Render.Primitives.quad Render.Shaders.fragment
                    { rotation = makeRotate pi (vec3 0 0 1)
                    , scale = makeScale3 (0.73 * radius) (0.73 * radius) 1
                    , color = Colour.white
                    , pos = vec3 (w * 0.5) (h * 0.5) 0
                    , worldRot = makeRotate 0 (vec3 0 0 1)
                    , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                    , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                    , texture = texture
                    }
                ]

            Nothing ->
                []
        ]
