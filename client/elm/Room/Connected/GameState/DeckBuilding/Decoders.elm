module DeckBuilding.Decoders exposing (character)

import DeckBuilding.Types exposing (Character)
import Json.Decode as Json exposing (Decoder, field, string)
import RuneSelect.Decoders


character : Decoder Character
character =
    Json.map5 Character
        (field "name" string)
        (field "img_url" string)
        (field "rune_a" RuneSelect.Decoders.rune)
        (field "rune_b" RuneSelect.Decoders.rune)
        (field "rune_c" RuneSelect.Decoders.rune)
