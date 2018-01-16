module Animation.Decoders exposing (decoder)

import Animation.Types exposing (Anim(..))
import Json.Decode as Json exposing (Decoder, fail, succeed)


decoder : String -> Decoder Anim
decoder s =
    case s of
        "slash" ->
            succeed Slash

        "obliterate" ->
            succeed Obliterate

        "heal" ->
            succeed Heal

        otherwise ->
            fail ("Invalid Anim " ++ s)
