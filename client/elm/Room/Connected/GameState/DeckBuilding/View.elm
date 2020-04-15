module DeckBuilding.View exposing (view)

import Background.View as Background
import Colour
import DeckBuilding.Messages exposing (Msg(..))
import DeckBuilding.Types exposing (Character, Model)
import Game.Entity as Game
import Game.State exposing (bareContextInit)
import Game.Types exposing (Context)
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (class, height, src, style, width)
import Html.Events exposing (onClick, onMouseEnter)
import Math.Matrix4 exposing (makeLookAt, makeOrtho, makeRotate, makeScale3)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (vec3)
import Maybe.Extra as Maybe
import Render.Primitives
import Render.Shaders
import Render.Types as Render
import Render.Uniforms exposing (uniColourMag)
import Texture.State as Texture
import Texture.Types as Texture
import Util exposing (px, to3d)
import WebGL
import WebGL.Texture as WebGL


view : Render.Params -> Model -> Texture.Model -> Html Msg
view { w, h, pixelRatio } ({ characters } as model) textures =
    let
        ctx =
            bareContextInit ( w, h ) textures
    in
    div [ class "character-select" ]
        [ WebGL.toHtml
            [ width <| floor <| toFloat w * pixelRatio, height <| floor <| toFloat h * pixelRatio, class "webgl-canvas" ]
          <|
            List.concat <|
                List.map ((|>) ctx)
                    []
        , h1 [] [ text "CHARACTER SELECT" ]
        , div [ class "characters" ] <|
            List.map (\{ name } -> text name) characters
        ]
