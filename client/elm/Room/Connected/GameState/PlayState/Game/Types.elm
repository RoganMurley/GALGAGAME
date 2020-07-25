module Game.Types exposing (ButtonEntity, Context, Entities, Feedback, HandEntity, Model, OtherHandEntity, StackEntity)

import Animation.Types exposing (Anim)
import Card.Types exposing (Card)
import Font.Types as Font
import Game.Entity as Game
import Hover exposing (HoverOther, HoverSelf)
import Main.Messages as Main
import Math.Matrix4 exposing (Mat4)
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
    , fonts : Font.Model
    , resolving : Bool
    , mouse : Maybe Vec2
    , perspective : Mat4
    , ortho : Mat4
    , camera : Mat4
    }


type alias StackEntity =
    Game.Entity3D
        { card : Card
        , index : Int
        , owner : WhichPlayer
        }


type alias HandEntity =
    Game.Entity3D
        { card : Card
        , index : Int
        , owner : WhichPlayer
        }


type alias OtherHandEntity =
    Game.Entity3D {}


type alias ButtonEntity =
    Game.Entity
        { font : String
        , text : String
        , onClick : Maybe Main.Msg
        , disabled : Bool
        , hover : Bool
        }


type alias Entities =
    { stack : List StackEntity
    , hand : List HandEntity
    , otherHand : List OtherHandEntity
    , buttons : List ButtonEntity
    }



-- Feedback


type alias Feedback =
    { progress : Float
    , pos : Vec2
    }
