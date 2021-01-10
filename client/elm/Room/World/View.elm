module World.View exposing (htmlView, webglView)

import Assets.Types as Assets
import Background.View as Background
import Buttons.View as Buttons
import Game.State exposing (bareContextInit)
import Html exposing (Html, div)
import Line
import Main.Types exposing (Flags)
import Math.Vector3 exposing (vec3)
import Vfx.State as Vfx
import WebGL
import World.Messages exposing (Msg)
import World.Types exposing (Model)


htmlView : Model -> Html Msg
htmlView _ =
    div [] []


webglView : Model -> Flags -> Assets.Model -> List WebGL.Entity
webglView { buttons, disabledButtons, time } { mouse, dimensions } assets =
    let
        ctx =
            bareContextInit dimensions assets mouse

        baseVfx =
            Vfx.init

        vfx =
            { baseVfx | rotation = time }

        { w, h, radius } =
            ctx

        start =
            { x = 0.5 * w + radius * 5 * (0.5 - 0.5)
            , y = 0.5 * h + radius * 5 * (0.33 - 0.5)
            }

        end =
            { x = 0.5 * w + radius * 5 * (0.4 - 0.5)
            , y = 0.5 * h + radius * 5 * (0.41 - 0.5)
            }

        line =
            { x1 = start.x
            , y1 = start.y
            , x2 = end.x
            , y2 = end.y
            }

        lineOptions =
            { col = vec3 0.96 0.95 0.37
            , thickness = 5
            , alpha = 0.8
            }
    in
    List.concat
        [ Background.radialView vfx ctx
        , Line.view line lineOptions ctx
        , Buttons.view buttons ctx
        , Buttons.view disabledButtons ctx
        ]
