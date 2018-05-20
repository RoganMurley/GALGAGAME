module Animation.Types exposing (..)

import Card.Types exposing (Card)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Stack.Types exposing (StackCard)
import WebGL exposing (Shader)
import WebGL.Texture exposing (Texture)
import WhichPlayer.Types exposing (WhichPlayer)


type alias Uniforms a =
    { a
        | time : Float
        , resolution : Vec2
        , flipper : Float
    }


type alias Textured a =
    { a | texture : Texture }


type FragShader
    = BaseShader (Shader {} (Uniforms {}) {})
    | TexturedShader (Shader {} (Uniforms (Textured {})) {})


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
    | Overdraw WhichPlayer
    | GameStart WhichPlayer
    | GameEnd (Maybe WhichPlayer)
    | Rotate WhichPlayer
    | Adhoc WhichPlayer ShaderName SfxUrl
    | Custom String


type alias SfxUrl =
    String


type alias ShaderName =
    String
