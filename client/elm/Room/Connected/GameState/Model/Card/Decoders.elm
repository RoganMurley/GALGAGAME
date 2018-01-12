module Card.Decoders exposing (..)

import Card.Types exposing (Anim(..), Card)
import Json.Decode as Json exposing (Decoder, fail, field, maybe, string, succeed)


decoder : Decoder Card
decoder =
    Json.map4 Card
        (field "name" string)
        (field "desc" string)
        (field "imageURL" string)
        (field "sfxURL" string)


animDecoder : String -> Decoder Anim
animDecoder s =
    case s of
        "slash" ->
            succeed Slash

        "obliterate" ->
            succeed Obliterate

        "heal" ->
            succeed Heal

        otherwise ->
            fail ("Invalid Anim " ++ s)
