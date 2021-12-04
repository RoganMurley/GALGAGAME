module Game.Types exposing (Context, Entities, Feedback, HandEntity, Model, OtherHandEntity, StackEntity, WheelEntity)

import Animation.Types exposing (Anim)
import Buttons.Types exposing (Buttons)
import Card.Types exposing (Card)
import Collision exposing (Ray)
import Font.Types as Font
import Game.Entity as Game
import Holding.Types exposing (Holding)
import Hover exposing (HoverOther, HoverSelf)
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Model.Types as Model
import Mouse exposing (MouseState(..))
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
    , buttons : Buttons
    , holding : Holding
    , timeLeft : Maybe Float
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
    , textures : Texture.Model
    , fonts : Font.Model
    , resolving : Bool
    , mouse : MouseState
    , mouseRay : Maybe Ray
    , perspective : Mat4
    , ortho : Mat4
    , camera2d : Mat4
    , camera3d : Mat4
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


type alias WheelEntity =
    Game.Entity3D {}


type alias Entities =
    { stack : List StackEntity
    , hand : List HandEntity
    , otherHand : List OtherHandEntity
    , wheel : List WheelEntity
    }



-- Feedback


type alias Feedback =
    { progress : Float
    , pos : Vec2
    }
