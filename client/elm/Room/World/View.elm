module World.View exposing (htmlView, webglView)

import Assets.Types as Assets
import Game.State exposing (bareContextInit)
import Html exposing (Html, div)
import Main.Types exposing (Flags)
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Render.Shaders
import Texture.State as Texture
import WebGL
import World.Messages exposing (Msg)
import World.Types exposing (Model)


htmlView : Model -> Html Msg
htmlView _ =
    div [] []


webglView : Model -> Flags -> Assets.Model -> List WebGL.Entity
webglView _ { mouse, dimensions } assets =
    let
        { perspective, camera3d, mouseRay } =
            bareContextInit dimensions assets mouse

        rotAxis =
            Maybe.withDefault (vec3 0 0 1) <|
                Maybe.map (Math.Vector3.negate << .direction) mouseRay
    in
    Texture.with assets.textures "kab.png" <|
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
