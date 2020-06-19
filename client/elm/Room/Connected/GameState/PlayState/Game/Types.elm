module Game.Types exposing (Context, Entities, Feedback, HandEntity, Hover(..), HoverBase, HoverOther, HoverSelf, HoverStack, Model, OtherHandEntity, StackEntity)

import Animation.Types exposing (Anim)
import Card.Types exposing (Card)
import Game.Entity as Game
import Math.Vector2 exposing (Vec2)
import Model.Types as Model
import Resolvable.Types as Resolvable
import Stack.Types exposing (StackCard)
import Texture.Types as Texture
import Vfx.Types as Vfx
import WhichPlayer.Types exposing (WhichPlayer)


type alias Model =
    { res : Resolvable.Model
    , focus : Maybe StackCard
    , hover : HoverSelf
    , otherHover : HoverOther
    , entities : Entities
    , passed : Bool
    , feedback : List Feedback
    , vfx : Vfx.Model
    }


type alias Context =
    { w : Float
    , h : Float
    , radius : Float
    , tick : Float
    , progress : Float
    , anim : Anim
    , animDamage : ( Float, Float )
    , model : Model.Model
    , stackCard : Maybe StackCard
    , textures : Texture.Model
    , resolving : Bool
    , mouse : Maybe Vec2
    }


type alias StackEntity =
    Game.Entity
        { card : Card
        , index : Int
        , owner : WhichPlayer
        }


type alias HandEntity =
    Game.Entity
        { card : Card
        , index : Int
        , owner : WhichPlayer
        }


type alias OtherHandEntity =
    Game.Entity {}


type alias Entities =
    { stack : List StackEntity
    , hand : List HandEntity
    , otherHand : List OtherHandEntity
    }



-- Hover


type alias HoverSelf =
    Hover { dmg : ( Model.Life, Model.Life ) }


type alias HoverOther =
    Hover {}


type alias HoverStack =
    Hover { dmg : ( Model.Life, Model.Life ) }


type alias HoverBase a =
    { a
        | index : Int
        , tick : Float
    }


type Hover a
    = HoverHand (HoverBase a)
    | HoverStack (HoverBase a)
    | NoHover



-- Feedback


type alias Feedback =
    { progress : Float
    , pos : Vec2
    }
