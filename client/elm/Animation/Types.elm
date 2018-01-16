module Animation.Types exposing (..)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)


type alias Uniforms =
    { time : Float
    , resolution : Vec2
    , flipper : Float
    }


type alias Vertex =
    { position : Vec3
    }


type Anim
    = Slash
    | Heal
    | Obliterate
    | Custom String
