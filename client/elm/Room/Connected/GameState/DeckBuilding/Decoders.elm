module DeckBuilding.Decoders exposing (character, choosingCharacter, runeChoice, runeNames)

import DeckBuilding.Types exposing (Character, ChoosingCharacter(..), RuneChoice)
import Json.Decode as Json exposing (Decoder, field, maybe, string)
import RuneSelect.Decoders


choosingCharacter : Decoder ChoosingCharacter
choosingCharacter =
    Json.oneOf
        [ Json.map ChosenCharacter (field "chosen" character)
        , Json.map UnchosenCharacter (field "unchosen" (maybe character))
        ]


character : Decoder Character
character =
    let
        makeCharacter : Maybe RuneChoice -> Character
        makeCharacter choice =
            Character choice 0
    in
    Json.map makeCharacter
        (field "choice" <| maybe runeChoice)


runeChoice : Decoder RuneChoice
runeChoice =
    Json.map3 RuneChoice
        (field "rune_a" RuneSelect.Decoders.rune)
        (field "rune_b" RuneSelect.Decoders.rune)
        (field "rune_c" RuneSelect.Decoders.rune)


runeNames : Decoder { a : String, b : String, c : String }
runeNames =
    Json.map3 (\a b c -> { a = a, b = b, c = c })
        (field "rune_a" string)
        (field "rune_b" string)
        (field "rune_c" string)
