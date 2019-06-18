module CharacterSelect.View exposing (view)

import Background.View as Background
import CharacterSelect.Messages exposing (Msg(..))
import CharacterSelect.State exposing (getHoverSlot)
import CharacterSelect.Types exposing (Character, Model, Slot(..))
import Colour
import Game.Entity as Game
import Game.State exposing (bareContextInit)
import Game.Types exposing (Context)
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (class, height, src, style, width)
import Html.Events exposing (onClick, onMouseEnter)
import Math.Matrix4 exposing (makeLookAt, makeOrtho, makeRotate, makeScale3)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (vec3)
import Maybe.Extra as Maybe
import Render.Primitives
import Render.Shaders
import Render.Types as Render
import Render.Uniforms exposing (uni, uniColourMag)
import Texture.State as Texture
import Texture.Types as Texture
import Util exposing (px, to3d)
import WebGL
import WebGL.Texture as WebGL


view : Render.Params -> Model -> Texture.Model -> Html Msg
view { w, h } ({ characters, selected, vm } as model) textures =
    let
        ctx =
            bareContextInit ( w, h ) textures

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
                [ img [ src ("/img/textures/" ++ character.imgURL), class "character-icon" ] []
                ]

        hoverCharacterNameView : Html msg
        hoverCharacterNameView =
            div
                [ class "hover-character-container" ]
                [ div
                    [ class "hover-character-name"
                    , style "top" (ctx.radius * 0.6 |> px)
                    ]
                    [ text vm.hover.name ]
                ]
    in
    div [ class "character-select" ]
        [ WebGL.toHtml
            [ width w, height h, class "webgl-canvas" ]
          <|
            List.concat <|
                List.map ((|>) ctx)
                    [ Background.backgroundView
                    , characterSelectCirclesView model
                    ]
        , h1 [] [ text "RUNE SELECT" ]
        , hoverCharacterNameView
        , div [ class "characters" ] <| List.map characterView characters
        ]


characterSelectCirclesView : Model -> Context -> List WebGL.Entity
characterSelectCirclesView ({ selected, vm } as model) ({ w, h, radius, textures } as ctx) =
    let
        centre =
            vec2 (w / 2) (h / 2)

        ( mTextureA, mTextureB, mTextureC ) =
            ( textureAt 0, textureAt 1, textureAt 2 )

        textureAt : Int -> Maybe WebGL.Texture
        textureAt i =
            Maybe.join <|
                Maybe.map (.imgURL >> Texture.load textures) <|
                    List.head <|
                        List.drop i <|
                            selected

        mTextureHover : Maybe WebGL.Texture
        mTextureHover =
            Texture.load textures vm.hover.imgURL

        hoverSlot : Maybe Slot
        hoverSlot =
            getHoverSlot model

        circleEntities : List (Game.Entity {})
        circleEntities =
            [ { scale = 0.24 * radius
              , position =
                    Math.Vector2.add
                        centre
                        (vec2 0 (-radius * 0.26))
              , rotation = 0
              }
            , { scale = 0.24 * radius
              , position =
                    Math.Vector2.add
                        centre
                        (vec2 (-radius * 0.23) (radius * 0.14))
              , rotation = 0
              }
            , { scale = 0.24 * radius
              , position =
                    Math.Vector2.add
                        centre
                        (vec2 (radius * 0.23) (radius * 0.14))
              , rotation = 0
              }
            ]
    in
    List.concat
        [ List.map
            (Render.Primitives.fullCircle << uniColourMag ctx Colour.darkGray 1)
            circleEntities
        , List.map
            (Render.Primitives.circle << uni ctx)
            circleEntities
        , case mTextureA of
            Just texture ->
                [ Render.Primitives.quad Render.Shaders.fragment
                    { rotation = makeRotate pi (vec3 0 0 1)
                    , scale = makeScale3 (0.15 * radius) (0.15 * radius) 1
                    , color = Colour.white
                    , pos = to3d <| slotView ctx SlotA
                    , worldRot = makeRotate 0 (vec3 0 0 1)
                    , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                    , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                    , texture = texture
                    }
                ]

            Nothing ->
                []
        , case mTextureB of
            Just texture ->
                [ Render.Primitives.quad Render.Shaders.fragment
                    { rotation = makeRotate pi (vec3 0 0 1)
                    , scale = makeScale3 (0.15 * radius) (0.15 * radius) 1
                    , color = Colour.white
                    , pos = to3d <| slotView ctx SlotB
                    , worldRot = makeRotate 0 (vec3 0 0 1)
                    , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                    , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                    , texture = texture
                    }
                ]

            Nothing ->
                []
        , case mTextureC of
            Just texture ->
                [ Render.Primitives.quad Render.Shaders.fragment
                    { rotation = makeRotate pi (vec3 0 0 1)
                    , scale = makeScale3 (0.15 * radius) (0.15 * radius) 1
                    , color = Colour.white
                    , pos = to3d <| slotView ctx SlotC
                    , worldRot = makeRotate 0 (vec3 0 0 1)
                    , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                    , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                    , texture = texture
                    }
                ]

            Nothing ->
                []
        , case mTextureHover of
            Just texture ->
                case hoverSlot of
                    Just slot ->
                        [ Render.Primitives.quad Render.Shaders.fragmentAlpha
                            { rotation = makeRotate pi (vec3 0 0 1)
                            , scale = makeScale3 (0.15 * radius) (0.15 * radius) 1
                            , alpha = 0.3
                            , color = Colour.white
                            , pos = to3d <| slotView ctx slot
                            , worldRot = makeRotate 0 (vec3 0 0 1)
                            , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                            , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                            , texture = texture
                            }
                        ]

                    _ ->
                        []

            Nothing ->
                []
        ]


slotView : Context -> Slot -> Vec2
slotView { w, h, radius } slot =
    let
        centre =
            vec2 (w / 2) (h / 2)

        offsetNormalised =
            case slot of
                SlotA ->
                    vec2 0 -0.26

                SlotB ->
                    vec2 -0.23 0.14

                SlotC ->
                    vec2 0.23 0.14

        offset =
            Math.Vector2.scale radius offsetNormalised
    in
    Math.Vector2.add centre offset
