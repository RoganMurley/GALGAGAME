module Clock.Entity exposing (..)

import Card.Types exposing (Card)
import Math.Vector2 exposing (Vec2)
import WhichPlayer.Types exposing (WhichPlayer(..))


type alias GameEntity a =
    { a
        | position : Vec2
        , rotation : Float
        , scale : Float
    }


type alias Entities =
    { stack : List (GameEntity { card : Card, owner : WhichPlayer })
    , hand : List (GameEntity { card : Card, owner : WhichPlayer })
    , otherHand : List (GameEntity {})
    }


init : Entities
init =
    { stack = []
    , hand = []
    , otherHand = []
    }
