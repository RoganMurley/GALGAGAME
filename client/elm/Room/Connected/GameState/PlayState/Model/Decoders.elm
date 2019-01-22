module Model.Decoders exposing (decoder)

import Card.Decoders as Card
import Json.Decode as Json exposing (Decoder, field, int, list)
import Model.Types exposing (Model)
import Stack.Decoders as Stack
import WhichPlayer.Decoders as WhichPlayer


decoder : Decoder Model
decoder =
    Json.map6 Model
        (field "handPA" <| list Card.decoder)
        (field "handPB" int)
        (field "stack" <| list Stack.stackCardDecoder)
        (field "turn" WhichPlayer.decoder)
        (field "lifePA" int)
        (field "lifePB" int)
