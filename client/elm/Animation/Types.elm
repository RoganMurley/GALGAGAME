module Animation.Types exposing (..)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Model.Types exposing (WhichPlayer)


type alias Uniforms =
    { time : Float
    , resolution : Vec2
    , flipper : Float
    }


type alias Vertex =
    { position : Vec3
    }


type Anim
    = Slash WhichPlayer
    | Heal WhichPlayer
    | Draw WhichPlayer
    | Obliterate WhichPlayer
    | Custom String
