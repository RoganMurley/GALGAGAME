module Game.Types exposing (Context, Entities, Feedback, HandEntity, Model, OtherHandEntity, StackEntity)

import Animation.Types exposing (Anim)
import Buttons.Types exposing (Buttons)
import Card.Types exposing (Card)
import Collision exposing (Ray)
import Font.Types as Font
import Game.Entity as Game
import Hover exposing (HoverOther, HoverSelf)
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
    , buttons : Buttons
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


type alias Entities =
    { stack : List StackEntity
    , hand : List HandEntity
    , otherHand : List OtherHandEntity
    }



-- Feedback


type alias Feedback =
    { progress : Float
    , pos : Vec2
    }
