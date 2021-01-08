module World.View exposing (htmlView, webglView)

import Assets.Types as Assets
import Background.View as Background
import Buttons.View as Buttons
import Game.State exposing (bareContextInit)
import Html exposing (Html, div)
import Main.Types exposing (Flags)
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

        -- rotAxis =
        --     Maybe.withDefault (vec3 0 0 1) <|
        --         Maybe.map
        --             (Math.Vector3.negate << .direction)
        --             ctx.mouseRay
        baseVfx =
            Vfx.init

        vfx =
            { baseVfx | rotation = time }
    in
    List.concat
        [ Background.radialView vfx ctx
        , Buttons.view buttons ctx
        , Buttons.view disabledButtons ctx
        ]
