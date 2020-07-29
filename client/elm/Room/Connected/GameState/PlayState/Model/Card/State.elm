module Card.State exposing (cardTexture, scale)

import Card.Types exposing (Card)
import Math.Vector3 exposing (Vec3, vec3)
import Texture.State as Texture
import Texture.Types as Texture
import WebGL.Texture as WebGL


cardTexture : Texture.Model -> Card -> Maybe WebGL.Texture
cardTexture textures { imgURL } =
    Texture.load textures imgURL


scale : Vec3
scale =
    vec3 0.125 0.125 1
