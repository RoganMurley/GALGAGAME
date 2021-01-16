module World.View exposing (htmlView, webglView)

import Assets.Types as Assets
import Background.View as Background
import Buttons.View as Buttons
import Game.State exposing (bareContextInit)
import Html exposing (Html, div)
import Line.View as Line
import Main.Types exposing (Flags)
import Math.Vector3 exposing (vec3)
import Vfx.State as Vfx
import WebGL
import World.Messages exposing (Msg)
import World.Types exposing (Model)
import World.WorldPos exposing (lineToWorldPos)


htmlView : Model -> Html Msg
htmlView _ =
    div [] []


webglView : Model -> Flags -> Assets.Model -> List WebGL.Entity
webglView model { mouse, dimensions } assets =
    let
        { world, encounterButtons, otherButtons, visitedButtons, time } =
            model

        ctx =
            bareContextInit dimensions assets mouse

        baseVfx =
            Vfx.init

        vfx =
            { baseVfx | rotation = time }

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
    List.concat
        [ Background.radialView vfx ctx
        , List.concat <| List.map (\edge -> Line.view (lineToWorldPos ctx edge) lineOptions ctx) world.edges
        , List.concat <| List.map (\edge -> Line.view (lineToWorldPos ctx edge) visitedLineOptions ctx) world.visitedEdges
        , Buttons.view encounterButtons ctx
        , Buttons.view otherButtons ctx
        , Buttons.view visitedButtons ctx
        ]
