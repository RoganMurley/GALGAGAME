module DeckBuilding.Decoders exposing (character, choosingCharacter)

import DeckBuilding.Types exposing (Character, ChoosingCharacter(..), RuneChoice)
import Json.Decode as Json exposing (Decoder, field, maybe)
import RuneSelect.Decoders


choosingCharacter : Decoder ChoosingCharacter
choosingCharacter =
    Json.oneOf
        [ Json.map ChosenCharacter (field "chosen" character)
        , Json.map UnchosenCharacter (field "unchosen" character)
        ]


character : Decoder Character
character =
    Json.map Character
        (field "choice" <| maybe runeChoice)


runeChoice : Decoder RuneChoice
runeChoice =
    Json.map3 RuneChoice
        (field "rune_a" RuneSelect.Decoders.rune)
        (field "rune_b" RuneSelect.Decoders.rune)
        (field "rune_c" RuneSelect.Decoders.rune)
