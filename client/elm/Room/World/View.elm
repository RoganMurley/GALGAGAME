module World.View exposing (htmlView, webglView)

import Assets.Types as Assets
import Background.View as Background
import Game.State exposing (bareContextInit)
import Html exposing (Html, div)
import Main.Types exposing (Flags)
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Render.Shaders
import Texture.State as Texture
import Vfx.State as Vfx
import WebGL
import World.Messages exposing (Msg)
import World.Types exposing (Model)


htmlView : Model -> Html Msg
htmlView _ =
    div [] []


webglView : Model -> Flags -> Assets.Model -> List WebGL.Entity
webglView { time } { mouse, dimensions } assets =
    let
        ctx =
            bareContextInit dimensions assets mouse

        { perspective, camera3d, mouseRay } =
            ctx

        rotAxis =
            Maybe.withDefault (vec3 0 0 1) <|
                Maybe.map (Math.Vector3.negate << .direction) mouseRay

        baseVfx =
            Vfx.init

        vfx =
            { baseVfx | rotation = time }
    in
    List.concat
        [ Background.radialView vfx ctx
        , Texture.with assets.textures "kab.png" <|
            \texture ->
                [ Render.Primitives.quad Render.Shaders.fragment
                    { rotation = makeRotate pi rotAxis
                    , scale = makeScale3 1 1 1
                    , color = vec3 1 1 1
                    , pos = vec3 0 0 1
                    , perspective = perspective
                    , camera = camera3d
                    , texture = texture
                    }
                ]
        ]
