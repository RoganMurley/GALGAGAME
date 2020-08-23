module Buttons.View exposing (view)

import Buttons.State as Buttons
import Buttons.Types exposing (Button, ButtonType(..), Buttons(..), ImageButtonParams, TextButtonOption(..), TextButtonParams)
import Dict
import Ease
import Font.View as Font
import Game.Types exposing (Context)
import List.Extra as List
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Render.Shaders
import Texture.State as Texture
import WebGL


view : Buttons -> Context -> List WebGL.Entity
view (Buttons buttons) ctx =
    List.concat <|
        List.map
            (\button -> buttonView button ctx)
            (Dict.values buttons)


buttonView : Button -> Context -> List WebGL.Entity
buttonView button ctx =
    case button.btn of
        TextButton textButton ->
            textButtonView button textButton ctx

        ImageButton imageButton ->
            imageButtonView button imageButton ctx


textButtonView : Button -> TextButtonParams -> Context -> List WebGL.Entity
textButtonView button params ctx =
    let
        { ortho, camera2d } =
            ctx

        { x, y, xScale, yScale, hover } =
            button

        { bgColor, textColor, font } =
            params

        hoverPop =
            10 * Ease.outQuint (button.hover / 300)

        text =
            if hover > 0 then
                hoverText

            else
                params.text

        hoverText =
            Buttons.getHoverText params

        circular =
            Buttons.isCircular params

        background =
            if circular then
                [ Render.Primitives.quad Render.Shaders.fullCircleFragment
                    { rotation = makeRotate pi (vec3 0 0 1)
                    , scale =
                        makeScale3
                            (0.06 * xScale + hoverPop)
                            (0.06 * yScale + hoverPop)
                            1
                    , color = bgColor
                    , pos = vec3 x y 0
                    , perspective = ortho
                    , camera = camera2d
                    , mag = 1
                    }
                ]

            else
                [ Render.Primitives.quad Render.Shaders.matte
                    { rotation = makeRotate pi (vec3 0 0 1)
                    , scale =
                        makeScale3
                            (0.04 * xScale + hoverPop)
                            (0.04 * yScale + hoverPop)
                            1
                    , color = bgColor
                    , alpha = 1
                    , pos = vec3 x y 0
                    , perspective = ortho
                    , camera = camera2d
                    }
                ]
    in
    List.concat
        [ background
        , Font.view
            font
            text
            { x = x
            , y = y
            , scaleX = 0.0001 * yScale + 0.001 * hoverPop
            , scaleY = 0.0001 * yScale + 0.001 * hoverPop
            , color = textColor
            }
            ctx
        ]


imageButtonView : Button -> ImageButtonParams -> Context -> List WebGL.Entity
imageButtonView button { img, color } ctx =
    let
        { ortho, camera2d, textures } =
            ctx

        { x, y, xScale, yScale, hover } =
            button

        hoverPop =
            10 * Ease.outQuint (hover / 300)
    in
    Texture.with textures img <|
        \texture ->
            [ Render.Primitives.quad Render.Shaders.fragment
                { rotation = makeRotate pi (vec3 0 0 1)
                , scale =
                    makeScale3
                        (0.04 * xScale + xScale * 0.001 * hoverPop)
                        (0.04 * yScale + yScale * 0.001 * hoverPop)
                        1
                , color = color
                , pos = vec3 x y 0
                , perspective = ortho
                , camera = camera2d
                , texture = texture
                }
            ]
