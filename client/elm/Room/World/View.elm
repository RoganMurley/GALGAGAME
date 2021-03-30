module World.View exposing (htmlView, webglView)

import Assets.Types as Assets
import Background.View as Background
import Buttons.View as Buttons
import Font.View as Font
import Game.State exposing (bareContextInit)
import Game.Types exposing (Context)
import Html exposing (Html, div)
import Line.View as Line
import Main.Types exposing (Flags)
import Math.Vector3 exposing (vec3)
import Vfx.State as Vfx
import WebGL
import World.Messages exposing (Msg)
import World.Types exposing (Decision, Model, World)
import World.WorldPos exposing (lineToWorldPos)


htmlView : Model -> Html Msg
htmlView _ =
    div [] []


webglView : Model -> Flags -> Assets.Model -> List WebGL.Entity
webglView model { mouse, dimensions } assets =
    let
        { world, choiceButtons, encounterButtons, otherButtons, visitedButtons, time } =
            model

        ctx =
            bareContextInit dimensions assets mouse

        baseVfx =
            Vfx.init

        vfx =
            { baseVfx | rotation = time }

        bodyView =
            case world.decision of
                Just decision ->
                    decisionView decision time ctx

                Nothing ->
                    linesView world ctx
    in
    List.concat
        [ Background.radialView vfx ctx
        , bodyView
        , Buttons.view encounterButtons ctx
        , Buttons.view otherButtons ctx
        , Buttons.view visitedButtons ctx
        , Buttons.view choiceButtons ctx
        ]


linesView : World -> Context -> List WebGL.Entity
linesView world ctx =
    let
        lineOptions =
            { col = vec3 0.96 0.95 0.37
            , thickness = 0.015 * ctx.radius
            , alpha = 1
            }

        visitedLineOptions =
            { col = vec3 0.96 0.95 0.37
            , thickness = 0.015 * ctx.radius
            , alpha = 1
            }
    in
    (List.concat <| List.map (\edge -> Line.view (lineToWorldPos ctx edge) lineOptions ctx) world.edges)
        ++ (List.concat <| List.map (\edge -> Line.view (lineToWorldPos ctx edge) visitedLineOptions ctx) world.visitedEdges)


decisionView : Decision -> Float -> Context -> List WebGL.Entity
decisionView { title, text } time ctx =
    let
        { w, h } =
            ctx

        size =
            1.4 * max w h
    in
    List.concat
        [ Font.view
            "Futura"
            title
            { x = w * 0.5
            , y = h * 0.2
            , scaleX = 0.0001 * size + 0.003 * sin (time * 0.005)
            , scaleY = 0.0001 * size + 0.003 * sin (time * 0.005)
            , color = vec3 (244 / 255) (241 / 255) (94 / 255)
            }
            ctx
        , Font.view
            "Futura"
            text
            { x = w * 0.5
            , y = h * 0.4
            , scaleX = 0.00005 * size + 0.001 * sin (time * 0.002)
            , scaleY = 0.00005 * size + 0.001 * sin (time * 0.002)
            , color = vec3 (244 / 255) (241 / 255) (94 / 255)
            }
            ctx
        ]
