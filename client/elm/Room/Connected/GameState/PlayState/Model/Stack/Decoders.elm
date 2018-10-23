module Stack.Decoders exposing (stackCardDecoder)

import Card.Decoders as Card
import Json.Decode as Json exposing (Decoder, field)
import Stack.Types exposing (StackCard)
import WhichPlayer.Decoders as WhichPlayer


stackCardDecoder : Decoder StackCard
stackCardDecoder =
    Json.map2 StackCard
        (field "owner" WhichPlayer.decoder)
        (field "card" Card.decoder)
