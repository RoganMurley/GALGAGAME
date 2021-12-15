module Card.State exposing (cardTexture, getCard, isRevealed, scale)

import Card.Types exposing (Card, KnowableCard(..))
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


getCard : KnowableCard -> Card
getCard knowable =
    case knowable of
        KnownCard card ->
            card

        UnknownCard card ->
            card


isRevealed : KnowableCard -> Bool
isRevealed knowable =
    case knowable of
        KnownCard _ ->
            True

        UnknownCard _ ->
            False
