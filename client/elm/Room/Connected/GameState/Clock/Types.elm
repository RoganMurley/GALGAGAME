module Clock.Types exposing (..)

import Card.Types exposing (Card)
import Clock.Entity exposing (Entities)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Resolvable.Types as Resolvable


type alias Model =
    { res : Resolvable.Model
    , focus : Maybe Card
    , mouse : Vec2
    , entities : Entities
    }


type alias Vertex =
    { position : Vec3
    , coord : Vec2
    }


type alias ClockParams =
    { w : Float
    , h : Float
    , radius : Float
    }


type alias GameEntity a =
    { a
        | position : Vec2
        , rotation : Float
        , scale : Float
    }
