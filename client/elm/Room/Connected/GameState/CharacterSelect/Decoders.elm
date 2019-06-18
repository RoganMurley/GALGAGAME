module CharacterSelect.Decoders exposing (character)

import Card.Decoders as Card
import CharacterSelect.Types exposing (Character, CharacterCards(..))
import Json.Decode as Json exposing (Decoder, field, index, string)


character : Decoder Character
character =
    Json.map3 Character
        (field "name" string)
        (field "img_url" string)
        (field "cards" characterCards)


characterCards : Decoder CharacterCards
characterCards =
    Json.map4 CharacterCards
        (index 0 Card.decoder)
        (index 1 Card.decoder)
        (index 2 Card.decoder)
        (index 3 Card.decoder)
