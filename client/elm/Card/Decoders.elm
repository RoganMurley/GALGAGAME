module Card.Decoders exposing (decoder)

import Card.Types exposing (Anim(..), Card)
import Json.Decode as Json exposing (Decoder, fail, field, maybe, string, succeed)


decoder : Decoder Card
decoder =
    Json.map5 Card
        (field "name" string)
        (field "desc" string)
        (field "imageURL" string)
        (field "sfxURL" string)
        (field "anim" <| maybe (string |> Json.andThen animDecoder))


animDecoder : String -> Decoder Anim
animDecoder s =
    case s of
        "slash" ->
            succeed Slash

        otherwise ->
            fail ("Invalid Anim " ++ s)
