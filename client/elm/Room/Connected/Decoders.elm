module Connected.Decoders exposing (decodeDamageOutcome)

import Hover exposing (HoverDamage)
import Json.Decode as Json exposing (Decoder, index)


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
