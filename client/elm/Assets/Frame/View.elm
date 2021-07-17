module Frame.View exposing (view)

import Font.Shaders
import Font.Types exposing (Entity)
import Game.Types exposing (Context)
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Texture.State as Texture
import WebGL
import WebGL.Texture


view : String -> Int -> Int -> Entity -> Context -> List WebGL.Entity
view textureName frameNum frameSize entity { camera2d, ortho, textures } =
    Texture.with textures textureName <|
        \texture ->
            let
                ( textureWidth, textureHeight ) =
                    WebGL.Texture.size texture

                width =
                    frameSize

                height =
                    frameSize

                framesPerLine =
                    textureWidth // width

                x =
                    modBy framesPerLine frameNum * width

                y =
                    (frameNum // framesPerLine) * height
            in
            [ Render.Primitives.quad Font.Shaders.char
                { rotation = makeRotate pi (vec3 0 0 1)
                , scale = makeScale3 (entity.scaleX * toFloat width) (entity.scaleY * toFloat height) 1
                , color = entity.color
                , pos = vec3 entity.x entity.y 0
                , perspective = ortho
                , camera = camera2d
                , texture = texture
                , x = toFloat x
                , y = toFloat y
                , width = toFloat width
                , height = toFloat height
                , textureWidth = toFloat textureWidth
                , textureHeight = toFloat textureHeight
                }
            ]
