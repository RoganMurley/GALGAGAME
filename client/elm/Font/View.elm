module Font.View exposing (view)

import Dict
import Font.Shaders
import Font.Types exposing (FontChar)
import Game.Types exposing (Context)
import List
import Math.Matrix4 exposing (makeLookAt, makeOrtho, makeRotate, makeScale3)
import Math.Vector3 exposing (Vec3, vec3)
import Render.Primitives
import Texture.State as Texture
import WebGL
import WebGL.Texture


view : String -> String -> { color : Vec3, scaleX : Float, scaleY : Float, x : Float, y : Float } -> Context -> List WebGL.Entity
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
                                (\char acc -> entity.scaleX * char.advance + acc)
                                0
                                chars

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
                                                (entity.x + offset - textWidth - entity.scaleX * originX + entity.scaleX * width)
                                                (entity.y - entity.scaleY * originY + entity.scaleY * height)
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
                            ( offset + entity.scaleX * width + entity.scaleX * advance, newEntities )
                    in
                    Tuple.second <|
                        List.foldl charView ( 0, [] ) chars

                Nothing ->
                    []
