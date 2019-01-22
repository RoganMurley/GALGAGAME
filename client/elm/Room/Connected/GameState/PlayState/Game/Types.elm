module Game.Types exposing (Context, Entities, HandEntity, Hover, Model, StackEntity)

import Animation.Types exposing (Anim)
import Card.Types exposing (Card)
import Game.Entity as Game
import Math.Vector2 exposing (Vec2)
import Model.Types as Model
import Resolvable.Types as Resolvable
import Stack.Types exposing (StackCard)
import Texture.Types as Texture
import WhichPlayer.Types exposing (WhichPlayer)


type alias Model =
    { res : Resolvable.Model
    , focus : Maybe StackCard
    , mouse : Maybe Vec2
    , hover : Hover { dmg : ( Model.Life, Model.Life ) }
    , otherHover : Hover {}
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


type alias StackEntity =
    Game.Entity
        { card : Card
        , owner : WhichPlayer
        }


type alias HandEntity =
    Game.Entity
        { card : Card
        , index : Int
        , owner : WhichPlayer
        }


type alias Entities =
    { stack : List StackEntity
    , hand : List HandEntity
    , otherHand : List (Game.Entity {})
    }


type alias Hover a =
    Maybe
        { a
            | index : Int
            , tick : Float
        }
