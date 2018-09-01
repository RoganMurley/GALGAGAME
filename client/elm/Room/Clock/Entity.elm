module Clock.Entity exposing (..)

import Math.Vector2 exposing (Vec2)
import WhichPlayer.Types exposing (WhichPlayer(..))


type alias GameEntity a =
    { a
        | position : Vec2
        , rotation : Float
        , scale : Float
    }


type alias Entities =
    { stack : List (GameEntity { owner : WhichPlayer })
    , hand : List (GameEntity {})
    , otherHand : List (GameEntity {})
    }


init : Entities
init =
    { stack = []
    , hand = []
    , otherHand = []
    }
