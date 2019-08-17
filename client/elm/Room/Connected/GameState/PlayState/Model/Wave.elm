module Model.Wave exposing (view)

import Animation.Types exposing (Anim(..), Hurt(..))
import Colour exposing (Colour)
import Game.Entity as Game
import Game.Types exposing (Context)
import Math.Vector2 exposing (vec2)
import Render.Primitives
import Render.Uniforms exposing (uniColourMag)
import Util exposing (interpFloat)
import WebGL


view : Context -> List WebGL.Entity
view ({ w, h, radius, anim, progress } as ctx) =
    let
        scale =
            interpFloat progress 0 (3 * radius)

        render : Colour -> Game.Entity {} -> WebGL.Entity
        render =
            \c e -> Render.Primitives.circle <| uniColourMag ctx c 1.0 e
    in
    case anim of
        Hurt _ _ _ ->
            [ render Colour.red
                { scale = scale
                , position = vec2 (w / 2) (h / 2)
                , rotation = 0
                }
            ]

        Heal _ _ ->
            [ render Colour.green
                { scale = scale
                , position = vec2 (w / 2) (h / 2)
                , rotation = 0
                }
            ]

        _ ->
            []
