module Card.Decoders exposing (decoder)

import Card.Types exposing (Card)
import Json.Decode as Json exposing (Decoder, field, string)


decoder : Decoder Card
decoder =
    Json.map4 Card
        (field "name" string)
        (field "desc" string)
        (field "imageURL" string)
        (field "sfxURL" string)
