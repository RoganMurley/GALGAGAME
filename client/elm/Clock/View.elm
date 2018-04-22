module Clock.View exposing (view)

import Clock.Types exposing (Model)
import Html exposing (..)
import Html.Attributes exposing (..)
import Main.Messages as Main
import Math.Matrix4 exposing (Mat4, transform)
import Math.Vector3 exposing (vec3)
import Clock.Meshes
import Clock.Shaders
import Clock.State exposing (uniforms)
import Raymarch.Types exposing (Params(..))
import Texture.State as Texture
import Texture.Types as Texture
import WebGL
import WebGL.Settings.Blend as WebGL


view : Params -> Model -> Texture.Model -> Html Main.Msg
view (Params _ ( w, h )) { time } textures =
    let
        theta =
            time / 1000

        downscale =
            1

        mTexture =
            Texture.load textures "sword"

        positions =
            Clock.State.clockFace 12 (vec3 0 0 0) 1
    in
        div [ class "clock" ]
            (case mTexture of
                Just texture ->
                    let
                        makeEntity index ( pos, rot ) =
                            WebGL.entityWith
                                [ WebGL.add WebGL.srcAlpha WebGL.oneMinusSrcAlpha ]
                                Clock.Shaders.vertex
                                Clock.Shaders.fragment
                                (mesh pos rot)
                                (uniforms theta ( w, h ) texture pos)

                        entities =
                            List.indexedMap makeEntity positions
                    in
                        [ WebGL.toHtml
                            [ width (w // downscale)
                            , height (h // downscale)
                            , class "raymarch-canvas"
                            ]
                            entities
                        ]

                Nothing ->
                    [ div [ class "error" ] [ text "Loading..." ] ]
            )


mesh : Math.Vector3.Vec3 -> Math.Matrix4.Mat4 -> WebGL.Mesh Clock.Types.Vertex
mesh pos rot =
    Clock.Meshes.quad pos rot 0.2
