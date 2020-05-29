module Render.Uniforms exposing (Uniforms, camera, perspective, uni, uniColour, uniColourMag, uniColourMagAlpha, worldRot)

import Colour exposing (Colour)
import Game.Entity as Game
import Game.Types exposing (Context)
import Math.Matrix4 exposing (Mat4, makeLookAt, makeOrtho, makeRotate, makeScale3)
import Math.Vector3 exposing (Vec3, vec3)
import Util exposing (to3d)


type alias Uniforms a =
    { a
        | rotation : Mat4
        , scale : Mat4
        , color : Vec3
        , pos : Vec3
        , worldRot : Mat4
        , perspective : Mat4
        , camera : Mat4
    }


perspective : Context -> Mat4
perspective { w, h } =
    makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000


camera : Mat4
camera =
    makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)


worldRot : Mat4
worldRot =
    makeRotate 0 <| vec3 0 0 1


uni : Context -> Game.Entity a -> Uniforms {}
uni ctx { position, rotation, scale } =
    { rotation = makeRotate rotation <| vec3 0 0 1
    , scale = makeScale3 scale scale 1
    , color = Colour.white
    , pos = to3d position
    , worldRot = makeRotate 0 <| vec3 0 0 1
    , perspective = perspective ctx
    , camera = camera
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
    , worldRot = makeRotate 0 <| vec3 0 0 1
    , perspective = perspective ctx
    , camera = camera
    , mag = mag
    }


uniColourMagAlpha : Context -> Colour -> Float -> Float -> Game.Entity a -> Uniforms { mag : Float, alpha : Float }
uniColourMagAlpha ctx colour mag alpha { position, rotation, scale } =
    { rotation = makeRotate rotation <| vec3 0 0 1
    , scale = makeScale3 scale scale 1
    , color = colour
    , pos = to3d position
    , worldRot = makeRotate 0 <| vec3 0 0 1
    , perspective = perspective ctx
    , camera = camera
    , mag = mag
    , alpha = alpha
    }
