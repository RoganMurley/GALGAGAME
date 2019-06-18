module Card.State exposing (cardTexture)

import Card.Types exposing (Card)
import Texture.State as Texture
import Texture.Types as Texture
import WebGL.Texture as WebGL


cardTexture : Texture.Model -> Card -> Maybe WebGL.Texture
cardTexture textures { imgURL } =
    Texture.load textures imgURL
