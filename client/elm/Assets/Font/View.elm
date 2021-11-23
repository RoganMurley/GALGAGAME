module Font.View exposing (view)

import Dict
import Font.Shaders
import Font.State exposing (getKerning)
import Font.Types exposing (Entity, FontChar, Line)
import Game.Types exposing (Context)
import List
import List.Extra as List
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Texture.State as Texture
import Util exposing (foldlWithPrev)
import WebGL
import WebGL.Texture


view : String -> String -> Entity -> Context -> List WebGL.Entity
view fontName text entity { camera2d, ortho, fonts, textures } =
    Texture.with textures fontName <|
        \texture ->
            case Dict.get fontName fonts.fonts of
                Just font ->
                    let
                        lines : List Line
                        lines =
                            List.map
                                (\( _, line ) -> List.filterMap (\char -> Dict.get char font.chars) line)
                                (List.groupWhile (\_ b -> b /= '\n') ('\n' :: String.toList text))

                        getLineWidth : Line -> Float
                        getLineWidth line =
                            foldlWithPrev
                                (\mPrevChar char acc ->
                                    entity.scaleX * (char.width + char.advance + getKerning font mPrevChar char) + acc
                                )
                                0
                                line

                        textHeight : Float
                        textHeight =
                            entity.scaleY * 256

                        ( textureWidth, textureHeight ) =
                            WebGL.Texture.size texture

                        charView : Int -> Float -> Maybe FontChar -> FontChar -> ( Float, List WebGL.Entity ) -> ( Float, List WebGL.Entity )
                        charView lineNum lineWidth mPrevFontChar fontChar ( offset, entities ) =
                            let
                                { x, y, width, height, originX, originY, advance, char } =
                                    fontChar

                                n =
                                    toFloat <| lineNum

                                xPos =
                                    offset + entity.x - 0.5 * lineWidth + entity.scaleX * (width - originX + kerning)

                                yPos =
                                    0.75 * textHeight + (n * 2 * textHeight) + entity.y - entity.scaleY * originY

                                kerning =
                                    getKerning font mPrevFontChar fontChar

                                newEntities =
                                    Render.Primitives.quad Font.Shaders.char
                                        { rotation = makeRotate pi (vec3 0 0 1)
                                        , scale = makeScale3 (entity.scaleX * width) (entity.scaleY * height) 1
                                        , color = entity.color
                                        , pos = vec3 xPos yPos 0
                                        , perspective = ortho
                                        , camera = camera2d
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
                            ( offset + entity.scaleX * (width + advance + kerning), newEntities )
                    in
                    List.concat <|
                        List.map Tuple.second <|
                            List.indexedMap
                                (\lineNum ( line, lineWidth ) ->
                                    foldlWithPrev (charView lineNum lineWidth) ( 0, [] ) line
                                )
                            <|
                                List.map (\line -> ( line, getLineWidth line )) lines

                Nothing ->
                    []
