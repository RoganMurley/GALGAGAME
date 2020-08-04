module Background.View exposing (radialView, stainView, view)

import Animation.Types exposing (Anim(..))
import Assets.Types as Assets
import Colour
import Game.State exposing (bareContextInit)
import Game.Types exposing (Context)
import Html exposing (Html, div)
import Html.Attributes exposing (class, height, width)
import Main.Types exposing (Flags)
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Render.Shaders
import Stack.Types exposing (StackCard)
import Texture.State as Texture
import Texture.Types as Texture
import Vfx.State as Vfx
import Vfx.Types as Vfx
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Flags -> Assets.Model -> Anim -> Html msg
view { dimensions, pixelRatio, time } assets anim =
    let
        ( w, h ) =
            dimensions

        baseCtx =
            bareContextInit dimensions assets Nothing

        ctx =
            { baseCtx | anim = anim, progress = time / 4000 }
    in
    div []
        [ WebGL.toHtml
            [ width <| floor <| toFloat w * pixelRatio
            , height <| floor <| toFloat h * pixelRatio
            , class "webgl-canvas"
            ]
          <|
            radialView Vfx.init ctx
        ]


stainView : Maybe StackCard -> Context -> List WebGL.Entity
stainView focus ({ camera2d, ortho, w, h, radius, textures } as ctx) =
    let
        rotation =
            getRingRotation ctx

        backing : List WebGL.Entity
        backing =
            case focus of
                Just { owner } ->
                    [ Render.Primitives.fullCircle
                        { rotation = makeRotate 0 (vec3 0 0 1)
                        , scale = makeScale3 (0.66 * radius) (0.66 * radius) 1
                        , color = Colour.background owner
                        , pos = vec3 (w * 0.5) (h * 0.5) 0
                        , perspective = ortho
                        , camera = camera2d
                        , mag = 1.0
                        }
                    ]

                Nothing ->
                    []
    in
    Texture.with textures "stain.png" <|
        \texture ->
            List.append
                backing
                [ Render.Primitives.quad Render.Shaders.fragment
                    { rotation = makeRotate rotation (vec3 0 0 1)
                    , scale = makeScale3 (0.8 * radius) (0.8 * radius) 1
                    , color = Colour.white
                    , pos = vec3 (w * 0.5) (h * 0.5) 0
                    , perspective = ortho
                    , camera = camera2d
                    , texture = texture
                    }
                ]


getRingRotation : Context -> Float
getRingRotation { anim, model, progress } =
    let
        rot =
            case anim of
                Rotate _ ->
                    toFloat model.rot - 1 + progress

                Windup _ ->
                    toFloat model.rot + (1 - progress)

                Finding ->
                    -12 * progress

                _ ->
                    toFloat model.rot
    in
    rot * -2.0 * pi / 12.0


radialView : Vfx.Model -> Context -> List WebGL.Entity
radialView { rotation } ({ camera2d, ortho, w, h, textures } as ctx) =
    let
        size =
            1.4 * max w h
    in
    Texture.with textures "radial.png" <|
        \texture ->
            [ Render.Primitives.quad Render.Shaders.tunnel
                { rotation = makeRotate pi (vec3 0 0 1)
                , scale = makeScale3 (0.5 * size) (0.5 * size) 1
                , color = Colour.white
                , pos = vec3 (w * 0.5) (h * 0.5) 0
                , perspective = ortho
                , camera = camera2d
                , spin = getRingRotation ctx
                , depth = rotation
                , texture = texture
                }
            ]
