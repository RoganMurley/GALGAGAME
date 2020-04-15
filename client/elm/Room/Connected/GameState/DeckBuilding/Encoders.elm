module DeckBuilding.Encoders exposing (encodeCharacter)

import DeckBuilding.Types exposing (Character)
import Json.Encode as Encode


encodeCharacter : Character -> String
encodeCharacter { name, rune_a, rune_b, rune_c } =
    let
        value : Encode.Value
        value =
            Encode.object
                [ ( "name", Encode.string name )
                , ( "rune_a", Encode.string rune_a.name )
                , ( "rune_b", Encode.string rune_b.name )
                , ( "rune_c", Encode.string rune_c.name )
                ]
    in
    Encode.encode 0 value
