module Model.Decoders exposing (..)

import Json.Decode as Json exposing (Decoder, fail, field, int, list, string, succeed)
import Card.Decoders as Card
import Model.Types exposing (..)
import Stack.Decoders as Stack
import WhichPlayer.Decoders as WhichPlayer


modelDecoder : Decoder Model
modelDecoder =
    Json.map6 (\a b c d e f -> Model a b c d e f Nothing)
        (field "handPA" <| list Card.decoder)
        (field "handPB" int)
        (field "stack" <| list Stack.stackCardDecoder)
        (field "turn" WhichPlayer.decoder)
        (field "lifePA" int)
        (field "lifePB" int)
