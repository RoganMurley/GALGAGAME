module RuneSelect.Decoders exposing (rune, runeCards)

import Card.Decoders as Card
import Json.Decode as Json exposing (Decoder, field, index, string)
import RuneSelect.Types exposing (Rune, RuneCards)


rune : Decoder Rune
rune =
    Json.map4 Rune
        (field "name" string)
        (field "desc" string)
        (field "img_url" string)
        (field "cards" runeCards)


runeCards : Decoder RuneCards
runeCards =
    Json.map4 RuneCards
        (index 0 Card.decoder)
        (index 1 Card.decoder)
        (index 2 Card.decoder)
        (index 3 Card.decoder)
