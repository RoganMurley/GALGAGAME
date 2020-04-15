module DeckBuilding.Decoders exposing (character, rune, runeCards)

import Card.Decoders as Card
import DeckBuilding.Types exposing (Character, Rune, RuneCards)
import Json.Decode as Json exposing (Decoder, field, index, string)


character : Decoder Character
character =
    Json.map4 Character
        (field "name" string)
        (field "rune_a" rune)
        (field "rune_b" rune)
        (field "rune_c" rune)


rune : Decoder Rune
rune =
    Json.map3 Rune
        (field "name" string)
        (field "img_url" string)
        (field "cards" runeCards)


runeCards : Decoder RuneCards
runeCards =
    Json.map4 RuneCards
        (index 0 Card.decoder)
        (index 1 Card.decoder)
        (index 2 Card.decoder)
        (index 3 Card.decoder)
