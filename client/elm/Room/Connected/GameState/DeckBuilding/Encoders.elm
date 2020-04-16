module DeckBuilding.Encoders exposing (encodeCharacter)

import DeckBuilding.Types exposing (Character)
import Json.Encode as Encode


encodeCharacter : Character -> String
encodeCharacter { name, runeA, runeB, runeC } =
    let
        value : Encode.Value
        value =
            Encode.object
                [ ( "character_name", Encode.string name )
                , ( "rune_a", Encode.string runeA.name )
                , ( "rune_b", Encode.string runeB.name )
                , ( "rune_c", Encode.string runeC.name )
                ]
    in
    Encode.encode 0 value
