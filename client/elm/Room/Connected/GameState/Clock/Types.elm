module Clock.Types exposing (..)

import Animation.Types exposing (Anim)
import Card.Types exposing (Card)
import Clock.Entity exposing (GameEntity)
import Math.Vector2 exposing (Vec2)
import Model.Types as Model
import Resolvable.Types as Resolvable
import Stack.Types exposing (StackCard)
import Texture.Types as Texture
import WhichPlayer.Types exposing (WhichPlayer)


type alias Model =
    { res : Resolvable.Model
    , focus : Maybe StackCard
    , mouse : Vec2
    , entities : Entities
    }


type alias Context =
    { w : Float
    , h : Float
    , radius : Float
    , tick : Float
    , progress : Float
    , anim : Anim
    , model : Model.Model
    , stackCard : Maybe StackCard
    , textures : Texture.Model
    }


type alias Entities =
    { stack :
        List
            (GameEntity
                { card : Card
                , owner : WhichPlayer
                }
            )
    , hand :
        List
            (GameEntity
                { card : Card
                , index : Int
                , owner : WhichPlayer
                }
            )
    , otherHand : List (GameEntity {})
    }
