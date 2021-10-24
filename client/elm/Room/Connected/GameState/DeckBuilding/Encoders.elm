module DeckBuilding.Encoders exposing (encodeCharacter)

import DeckBuilding.Types exposing (Character)
import Json.Encode as Encode


encodeCharacter : Character -> String
encodeCharacter { choice } =
    let
        value : Encode.Value
        value =
            case choice of
                Just { runeA, runeB, runeC } ->
                    Encode.object
                        [ ( "rune_a", Encode.string runeA.name )
                        , ( "rune_b", Encode.string runeB.name )
                        , ( "rune_c", Encode.string runeC.name )
                        ]

                Nothing ->
                    Encode.object []
    in
    Encode.encode 0 value
