module Animation.Types exposing (..)

import Card.Types exposing (Card)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Stack.Types exposing (StackCard)
import WebGL.Texture exposing (Texture)
import WhichPlayer.Types exposing (WhichPlayer)


type alias Uniforms =
    { time : Float
    , resolution : Vec2
    , flipper : Float
    , texture : Texture
    }


type alias Vertex =
    { position : Vec3
    }


type Anim
    = Slash WhichPlayer Int
    | Heal WhichPlayer
    | Draw WhichPlayer
    | Bite WhichPlayer Int
    | Reverse WhichPlayer
    | Obliterate WhichPlayer
    | Play WhichPlayer Card
    | Transmute WhichPlayer StackCard StackCard
    | GameEnd (Maybe WhichPlayer)
    | Adhoc WhichPlayer ShaderName SfxUrl
    | Custom String


type alias SfxUrl =
    String


type alias ShaderName =
    String
