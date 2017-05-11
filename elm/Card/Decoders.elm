module Card.Decoders exposing (decoder)

import Card.Types exposing (Card)
import Json.Decode exposing (Decoder, field, map4, string)


decoder : Decoder Card
decoder =
    map4 Card
        (field "name" string)
        (field "desc" string)
        (field "imageURL" string)
        (field "sfxURL" string)
