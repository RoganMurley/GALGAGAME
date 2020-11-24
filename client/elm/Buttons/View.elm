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

        { x, y, width, height, hover } =
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
                            (width + hoverPop)
                            (height + hoverPop)
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
                            (width + hoverPop)
                            (height + hoverPop)
                            1
                    , color = bgColor
                    , alpha = 1
                    , pos = vec3 x y 0
                    , perspective = ortho
                    , camera = camera2d
                    }
                ]

        additionalTextScale =
            if circular then
                0.65

            else
                1
    in
    List.concat
        [ background
        , Font.view
            font
            text
            { x = x
            , y = y
            , scaleX = additionalTextScale * 0.0025 * (height + 0.6 * hoverPop)
            , scaleY = additionalTextScale * 0.0025 * (height + 0.6 * hoverPop)
            , color = textColor
            }
            ctx
        ]


imageButtonView : Button -> ImageButtonParams -> Context -> List WebGL.Entity
imageButtonView button { img, color } ctx =
    let
        { ortho, camera2d, textures } =
            ctx

        { x, y, width, height, hover } =
            button

        hoverPop =
            10 * Ease.outQuint (hover / 300)

        matchSign a b =
            if a >= 0 then
                b

            else
                -b
    in
    Texture.with textures img <|
        \texture ->
            [ Render.Primitives.quad Render.Shaders.fragment
                { rotation = makeRotate pi (vec3 0 0 1)
                , scale =
                    makeScale3
                        (width + matchSign width hoverPop)
                        (height + matchSign height hoverPop)
                        1
                , color = color
                , pos = vec3 x y 0
                , perspective = ortho
                , camera = camera2d
                , texture = texture
                }
            ]
