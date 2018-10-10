module Model.Wave exposing (..)

import Animation.Types exposing (Anim(..))
import Colour exposing (Colour)
import Game.Entity as Game
import Game.Types exposing (Context)
import Math.Vector2 exposing (vec2)
import Util exposing (interpFloat)
import Render.Primitives
import Render.Uniforms exposing (uniColour)
import WebGL


view : Context -> List WebGL.Entity
view ({ w, h, radius, anim } as ctx) =
    let
        progress =
            case anim of
                Heal _ ->
                    1 - ctx.progress

                _ ->
                    ctx.progress

        scale =
            interpFloat progress 0 (3 * radius)

        render : Colour -> Game.Entity {} -> WebGL.Entity
        render =
            \c e -> Render.Primitives.circle <| uniColour ctx c e
    in
        case anim of
            Slash _ _ ->
                [ render Colour.red
                    { scale = scale
                    , position = vec2 (w / 2) (h / 2)
                    , rotation = 0
                    }
                ]

            Bite _ _ ->
                [ render Colour.red
                    { scale = scale
                    , position = vec2 (w / 2) (h / 2)
                    , rotation = 0
                    }
                ]

            Heal _ ->
                [ render Colour.green
                    { scale = scale
                    , position = vec2 (w / 2) (h / 2)
                    , rotation = 0
                    }
                ]

            _ ->
                []
