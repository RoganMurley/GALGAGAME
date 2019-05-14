module Background.View exposing (backgroundView, cursorView, ornateView, ringView, view)

import Animation.State as Animation
import Animation.Types exposing (Anim(..))
import Colour
import Game.State exposing (bareContextInit)
import Game.Types exposing (Context)
import Html exposing (Html, div)
import Html.Attributes exposing (class, height, width)
import Main.Types exposing (Flags)
import Math.Matrix4 exposing (makeLookAt, makeOrtho, makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Render.Shaders
import Render.Uniforms
import Texture.State as Texture
import Texture.Types as Texture
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Flags -> Texture.Model -> Anim -> Html msg
view { dimensions, time } textures anim =
    let
        ( w, h ) =
            dimensions

        baseCtx =
            bareContextInit dimensions textures

        ctx =
            { baseCtx | anim = anim, progress = time / 4000 }
    in
    div []
        [ WebGL.toHtml [ width w, height h, class "webgl-canvas" ] <|
            backgroundView ctx
        ]


backgroundView : Context -> List WebGL.Entity
backgroundView ctx =
    List.concat <|
        List.map ((|>) ctx)
            [ ornateView, ringView, cursorView ]


ornateView : Context -> List WebGL.Entity
ornateView ({ w, h, anim, tick } as ctx) =
    let
        color =
            Colour.darkGray

        scale =
            0.75

        shake =
            0.005 * (Animation.animShake anim PlayerB tick - Animation.animShake anim PlayerA tick)

        render rotation =
            [ Render.Primitives.quad Render.Shaders.ornate
                { rotation = makeRotate (rotation + shake) <| vec3 0 0 1
                , scale = makeScale3 (scale * w) (scale * h) 1
                , color = color
                , pos = vec3 (w * 0.5) (h * 0.5) 0
                , worldRot = makeRotate 0 <| vec3 0 0 1
                , perspective = Render.Uniforms.perspective ctx
                , camera = Render.Uniforms.camera
                , shift = 0
                , frequency = 2
                , amplitude = 0.2
                , thickness = 0.02
                }
            , Render.Primitives.quad Render.Shaders.ornate
                { rotation = makeRotate (rotation + shake) <| vec3 0 0 1
                , scale = makeScale3 (scale * w) (-scale * h) 1
                , color = color
                , pos = vec3 (w * 0.5) (h * 0.5) 0
                , worldRot = makeRotate 0 <| vec3 0 0 1
                , perspective = Render.Uniforms.perspective ctx
                , camera = Render.Uniforms.camera
                , shift = 0
                , frequency = 2
                , amplitude = 0.2
                , thickness = 0.02
                }
            ]
    in
    List.concat <|
        List.map render <|
            List.map ((*) pi) <|
                [ 0, 1.0 ]


ringView : Context -> List WebGL.Entity
ringView { w, h, anim, progress, radius, textures } =
    let
        rotationProgress =
            case anim of
                Rotate _ ->
                    progress

                Windup _ ->
                    1 - progress

                Finding ->
                    -12 * progress

                _ ->
                    0

        rotation =
            rotationProgress * -2.0 * pi / 12.0

        ringEntity =
            case Texture.load textures "ring.png" of
                Nothing ->
                    []

                Just texture ->
                    [ Render.Primitives.quad Render.Shaders.fragment
                        { rotation = makeRotate rotation (vec3 0 0 1)
                        , scale = makeScale3 (0.77 * radius) (0.77 * radius) 1
                        , color = Colour.white
                        , pos = vec3 (w * 0.5) (h * 0.5) 0
                        , worldRot = makeRotate 0 (vec3 0 0 1)
                        , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                        , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                        , texture = texture
                        }
                    ]

        lifeclawEntities =
            case Texture.load textures "lifeclaw.png" of
                Nothing ->
                    []

                Just texture ->
                    [ Render.Primitives.quad Render.Shaders.fragment
                        { rotation = makeRotate pi (vec3 0 0 1)
                        , scale = makeScale3 (0.21 * radius) (0.21 * radius) 1
                        , color = Colour.white
                        , pos = vec3 (w * 0.5 + 0.65 * radius) (h * 0.5 - 0.65 * radius) 0
                        , worldRot = makeRotate 0 (vec3 0 0 1)
                        , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                        , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                        , texture = texture
                        }
                    , Render.Primitives.quad Render.Shaders.fragment
                        { rotation = makeRotate 0 (vec3 0 0 1)
                        , scale = makeScale3 (0.21 * radius) (0.21 * radius) 1
                        , color = Colour.white
                        , pos = vec3 (w * 0.5 - 0.65 * radius) (h * 0.5 + 0.65 * radius) 0
                        , worldRot = makeRotate 0 (vec3 0 0 1)
                        , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                        , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                        , texture = texture
                        }
                    , Render.Primitives.quad Render.Shaders.fragment
                        { rotation = makeRotate pi (vec3 0 0 1)
                        , scale = makeScale3 (0.17 * radius) (-0.17 * radius) 1
                        , color = Colour.white
                        , pos = vec3 (w * 0.5 + 0.65 * radius) (h * 0.5 + 0.62 * radius) 0
                        , worldRot = makeRotate 0 (vec3 0 0 1)
                        , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                        , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                        , texture = texture
                        }
                    , Render.Primitives.quad Render.Shaders.fragment
                        { rotation = makeRotate pi (vec3 0 0 1)
                        , scale = makeScale3 (-0.17 * radius) (0.17 * radius) 1
                        , color = Colour.white
                        , pos = vec3 (w * 0.5 - 0.65 * radius) (h * 0.5 - 0.62 * radius) 0
                        , worldRot = makeRotate 0 (vec3 0 0 1)
                        , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                        , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                        , texture = texture
                        }
                    ]

        bgEntity =
            [ Render.Primitives.quad Render.Shaders.fullCircleFragment
                { rotation = makeRotate rotation (vec3 0 0 1)
                , scale = makeScale3 (0.86 * radius) (0.86 * radius) 1
                , color = vec3 0.38 0.38 0.38
                , pos = vec3 (w * 0.5) (h * 0.5) 0
                , worldRot = makeRotate 0 (vec3 0 0 1)
                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                , mag = 1.0
                }
            ]
    in
    bgEntity ++ lifeclawEntities ++ ringEntity


cursorView : Context -> List WebGL.Entity
cursorView { w, h, radius, textures } =
    case Texture.load textures "cursor.png" of
        Nothing ->
            []

        Just texture ->
            [ Render.Primitives.quad Render.Shaders.fragment
                { rotation = makeRotate pi (vec3 0 0 1)
                , scale = makeScale3 (0.18 * radius) (-0.18 * radius) 1
                , color = Colour.white
                , pos = vec3 (w * 0.5) (h * 0.5 - 0.62 * radius) 0
                , worldRot = makeRotate 0 (vec3 0 0 1)
                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                , texture = texture
                }
            ]
