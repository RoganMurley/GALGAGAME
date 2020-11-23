module Stack.Decoders exposing (decoder, stackCardDecoder)

import Card.Decoders as Card
import Json.Decode as Json exposing (Decoder, field, maybe)
import Stack.Types exposing (Stack, StackCard)
import Wheel.Decoders as Wheel
import WhichPlayer.Decoders as WhichPlayer


decoder : Decoder Stack
decoder =
    Wheel.decoder (maybe stackCardDecoder)


stackCardDecoder : Decoder StackCard
stackCardDecoder =
    Json.map2 StackCard
        (field "owner" WhichPlayer.decoder)
        (field "card" Card.decoder)
