module DeckBuilding.View exposing (webglView)

import Assets.Types as Assets
import Background.View exposing (radialView)
import Carousel exposing (Carousel)
import DeckBuilding.Messages exposing (Msg(..))
import DeckBuilding.Types exposing (Button, Character, Model)
import Ease
import Font.Types as Font
import Font.View as Font
import Game.State exposing (bareContextInit)
import Game.Types exposing (Context)
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Render.Shaders
import Render.Types as Render
import RuneSelect.Types as RuneSelect exposing (RuneCursor(..))
import RuneSelect.View as RuneSelect
import WebGL
import WebGL.Texture as WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


webglView : Render.Params -> Model -> Assets.Model -> List WebGL.Entity
webglView { w, h } model assets =
    let
        ctx =
            bareContextInit ( w, h ) assets Nothing
    in
    (List.concat <|
        List.map ((|>) ctx) <|
            case model.runeSelect of
                Just runeSelect ->
                    [ RuneSelect.webglView runeSelect ]

                Nothing ->
                    [ radialView model.vfx

                    -- , titleView model.vfx.rotation
                    , charactersView model.characters model.vfx.rotation
                    ]
    )
        ++ (case model.buttons.ready of
                Just button ->
                    buttonView button model.vfx.rotation ctx

                Nothing ->
                    []
           )


charactersView : Carousel Character -> Float -> Context -> List WebGL.Entity
charactersView characters tick ctx =
    let
        character =
            characters.selected

        { w, h } =
            ctx

        size =
            1.4 * max w h
    in
    List.concat
        [ Font.view
            "Futura"
            character.name
            { x = w * 0.5 - 0.003 * size
            , y = h * 0.2
            , scaleX = 0.0001 * size + 0.003 * sin (tick * 0.005)
            , scaleY = 0.0001 * size + 0.003 * sin (tick * 0.007)
            , color = vec3 (244 / 255) (241 / 255) (94 / 255)
            }
            ctx
        ]


buttonView : Button -> Float -> Context -> List WebGL.Entity
buttonView button _ ctx =
    let
        { ortho, camera2d } =
            ctx

        ( textColor, bgColor ) =
            if button.hover > 0 then
                ( vec3 (0 / 255) (0 / 255) (80 / 255), vec3 (255 / 255) (255 / 255) (0 / 255) )

            else
                ( vec3 (0 / 255) (0 / 255) (80 / 255), vec3 (244 / 255) (241 / 255) (94 / 255) )

        hoverPop =
            10 * Ease.outQuint (button.hover / 300)

        buttonText =
            if button.hover > 0 then
                "READY!"

            else
                "READY?"
    in
    List.concat
        [ [ Render.Primitives.quad Render.Shaders.matte
                { rotation = makeRotate pi (vec3 0 0 1)
                , scale =
                    makeScale3
                        (0.12 * button.size + hoverPop)
                        (0.05 * button.size + hoverPop)
                        1
                , color = bgColor
                , alpha = 1
                , pos = vec3 button.x button.y 0
                , perspective = ortho
                , camera = camera2d
                }
          ]
        , Font.view
            "Futura"
            buttonText
            { x = button.x
            , y = button.y
            , scaleX = 0.0001 * button.size + 0.001 * hoverPop
            , scaleY = 0.0001 * button.size + 0.001 * hoverPop
            , color = textColor
            }
            ctx
        ]



-- titleView : Float -> Context -> List WebGL.Entity
-- titleView tick ({ w, h } as ctx) =
--     let
--         size =
--             1.4 * max w h
--     in
--     List.concat
--         [ Font.view
--             "Futura"
--             "GALGAGAME"
--             { x = w * 0.5 - 0.003 * size
--             , y = h * 0.5
--             , scaleX = 0.0001 * size + 0.003 * sin (tick * 0.005)
--             , scaleY = 0.0001 * size + 0.003 * sin (tick * 0.007)
--             , color = vec3 (20 / 255) (20 / 255) (20 / 255)
--             }
--             ctx
--         , Font.view
--             "Futura"
--             "GALGAGAME"
--             { x = w * 0.5
--             , y = h * 0.5
--             , scaleX = 0.0001 * size + 0.003 * sin (tick * 0.005)
--             , scaleY = 0.0001 * size + 0.003 * sin (tick * 0.007)
--             , color = vec3 (244 / 255) (241 / 255) (94 / 255)
--             }
--             ctx
--         ]
