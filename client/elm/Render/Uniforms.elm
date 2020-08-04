module Render.Uniforms exposing (Uniforms, camera2d, camera3d, ortho, perspective, uni, uniColour, uniColourMag)

import Colour exposing (Colour)
import Game.Entity as Game
import Game.Types exposing (Context)
import Math.Matrix4 exposing (Mat4, makeLookAt, makeOrtho, makePerspective, makeRotate, makeScale3)
import Math.Vector3 exposing (Vec3, vec3)
import Util exposing (to3d)


type alias Uniforms a =
    { a
        | rotation : Mat4
        , scale : Mat4
        , color : Vec3
        , pos : Vec3
        , perspective : Mat4
        , camera : Mat4
    }


ortho : { ctx | w : Float, h : Float } -> Mat4
ortho { w, h } =
    makeOrtho 0 (w / 2) (h / 2) 0 0 1


perspective : { ctx | w : Float, h : Float } -> Mat4
perspective { w, h } =
    makePerspective 45 (w / h) 0.01 100


camera2d : Mat4
camera2d =
    makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)


camera3d : Mat4
camera3d =
    makeLookAt (vec3 0 0 -1) (vec3 0 0 0) (vec3 0 1 0)


uni : Context -> Game.Entity a -> Uniforms {}
uni ctx { position, rotation, scale } =
    { rotation = makeRotate rotation <| vec3 0 0 1
    , scale = makeScale3 scale scale 1
    , color = Colour.white
    , pos = to3d position
    , perspective = ctx.ortho
    , camera = ctx.camera2d
    }


uniColour : Context -> Colour -> Game.Entity a -> Uniforms {}
uniColour ctx colour entity =
    let
        u =
            uni ctx entity
    in
    { u | color = colour }


uniColourMag : Context -> Colour -> Float -> Game.Entity a -> Uniforms { mag : Float }
uniColourMag ctx colour mag { position, rotation, scale } =
    { rotation = makeRotate rotation <| vec3 0 0 1
    , scale = makeScale3 scale scale 1
    , color = colour
    , pos = to3d position
    , perspective = ctx.ortho
    , camera = ctx.camera2d
    , mag = mag
    }
