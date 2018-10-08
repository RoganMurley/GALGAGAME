module Clock.Types exposing (..)

import Animation.Types exposing (Anim)
import Clock.Entity exposing (Entities)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Model.Types as Model
import Resolvable.Types as Resolvable
import Stack.Types exposing (StackCard)
import Texture.Types as Texture


type alias Model =
    { res : Resolvable.Model
    , focus : Maybe StackCard
    , mouse : Vec2
    , entities : Entities
    }


type alias Vertex =
    { position : Vec3
    , coord : Vec2
    }


type alias Context =
    { w : Float
    , h : Float
    , radius : Float
    , tick : Float
    , progress : Float
    , anim : Maybe Anim
    , model : Model.Model
    , stackCard : Maybe StackCard
    , textures : Texture.Model
    }


type alias GameEntity a =
    { a
        | position : Vec2
        , rotation : Float
        , scale : Float
    }
