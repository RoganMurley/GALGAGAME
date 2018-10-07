module Model.Decoders exposing (..)

import Json.Decode as Json exposing (Decoder, field, int, list)
import Card.Decoders as Card
import Model.Types exposing (Model)
import Stack.Decoders as Stack
import WhichPlayer.Decoders as WhichPlayer


decoder : Decoder Model
decoder =
    Json.map6 (\a b c d e f -> Model a b c d e f Nothing)
        (field "handPA" <| list Card.decoder)
        (field "handPB" int)
        (field "stack" <| list Stack.stackCardDecoder)
        (field "turn" WhichPlayer.decoder)
        (field "lifePA" int)
        (field "lifePB" int)
