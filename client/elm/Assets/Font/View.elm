module Font.View exposing (view)

import Dict
import Font.Shaders
import Font.Types exposing (Entity, FontChar)
import Game.Types exposing (Context)
import List
import Math.Matrix4 exposing (makeLookAt, makeOrtho, makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Texture.State as Texture
import WebGL
import WebGL.Texture


view : String -> String -> Entity -> Context -> List WebGL.Entity
view fontName text entity { fonts, textures, w, h } =
    Texture.with textures fontName <|
        \texture ->
            case Dict.get fontName fonts.fonts of
                Just font ->
                    let
                        chars : List FontChar
                        chars =
                            List.filterMap
                                (\char -> Dict.get char font)
                                (String.toList text)

                        textWidth : Float
                        textWidth =
                            List.foldl
                                (\char acc -> entity.scaleX * (char.width + char.advance) + acc)
                                0
                                chars

                        textHeight : Float
                        textHeight =
                            entity.scaleY * 256

                        ( textureWidth, textureHeight ) =
                            WebGL.Texture.size texture

                        charView : FontChar -> ( Float, List WebGL.Entity ) -> ( Float, List WebGL.Entity )
                        charView { x, y, width, height, originX, originY, advance } ( offset, entities ) =
                            let
                                newEntities =
                                    Render.Primitives.quad Font.Shaders.char
                                        { rotation = makeRotate pi (vec3 0 0 1)
                                        , scale = makeScale3 (entity.scaleX * width) (entity.scaleY * height) 1
                                        , color = entity.color
                                        , pos =
                                            vec3
                                                (offset + entity.x - 0.5 * textWidth + entity.scaleX * (width - originX))
                                                (0.5 * textHeight + entity.y - entity.scaleY * originY)
                                                0
                                        , worldRot = makeRotate 0 (vec3 0 0 1)
                                        , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                                        , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                                        , texture = texture
                                        , x = x
                                        , y = y
                                        , width = width
                                        , height = height
                                        , textureWidth = toFloat textureWidth
                                        , textureHeight = toFloat textureHeight
                                        }
                                        :: entities
                            in
                            ( offset + entity.scaleX * (width + advance), newEntities )
                    in
                    Tuple.second <|
                        List.foldl charView ( 0, [] ) chars

                Nothing ->
                    []
