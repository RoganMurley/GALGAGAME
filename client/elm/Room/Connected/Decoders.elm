module Connected.Decoders exposing (decodeDamageOutcome, decodePlayers)

import Connected.Types exposing (Players)
import Hover exposing (HoverDamage)
import Json.Decode as Json exposing (Decoder, index, maybe, string)


decodePlayers : String -> Result Json.Error Players
decodePlayers msg =
    Json.decodeString playersDecoder msg


playersDecoder : Decoder Players
playersDecoder =
    Json.map2 Players
        (index 0 <| maybe string)
        (index 1 <| maybe string)


decodeDamageOutcome : String -> Result Json.Error ( HoverDamage, HoverDamage )
decodeDamageOutcome msg =
    let
        decoder : Decoder ( HoverDamage, HoverDamage )
        decoder =
            Json.map2 (\a b -> ( a, b ))
                (index 0 Hover.damageDecoder)
                (index 1 Hover.damageDecoder)
    in
    Json.decodeString decoder msg
