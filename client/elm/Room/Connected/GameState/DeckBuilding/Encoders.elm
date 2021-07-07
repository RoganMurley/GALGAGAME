module DeckBuilding.Encoders exposing (encodeCharacter)

import DeckBuilding.Types exposing (Character)
import Json.Encode as Encode


encodeCharacter : Character -> String
encodeCharacter { name, choice } =
    let
        value : Encode.Value
        value =
            case choice of
                Just { runeA, runeB, runeC } ->
                    Encode.object
                        [ ( "character_name", Encode.string name )
                        , ( "rune_a", Encode.string runeA.name )
                        , ( "rune_b", Encode.string runeB.name )
                        , ( "rune_c", Encode.string runeC.name )
                        ]

                Nothing ->
                    Encode.object
                        [ ( "character_name", Encode.string name )
                        ]
    in
    Encode.encode 0 value
