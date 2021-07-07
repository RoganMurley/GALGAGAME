module DeckBuilding.Decoders exposing (character)

import DeckBuilding.Types exposing (Character, RuneChoice)
import Json.Decode as Json exposing (Decoder, field, maybe, string)
import RuneSelect.Decoders


character : Decoder Character
character =
    Json.map3 Character
        (field "name" string)
        (field "img_url" string)
        (field "choice" <| maybe runeChoice)


runeChoice : Decoder RuneChoice
runeChoice =
    Json.map3 RuneChoice
        (field "rune_a" RuneSelect.Decoders.rune)
        (field "rune_b" RuneSelect.Decoders.rune)
        (field "rune_c" RuneSelect.Decoders.rune)
