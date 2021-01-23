module Model.Decoders exposing (decoder)

import Card.Decoders as Card
import Json.Decode as Json exposing (Decoder, field, int, list)
import Model.Types exposing (Model)
import Stack.Decoders as Stack
import WhichPlayer.Decoders as WhichPlayer


decoder : Decoder Model
decoder =
    Json.map8 (<|)
        (Json.map2 Model
            (field "handPA" <| list Card.decoder)
            (field "handPB" int)
        )
        (field "stack" Stack.decoder)
        (field "turn" WhichPlayer.decoder)
        (field "lifePA" int)
        (field "lifePB" int)
        (field "maxLifePA" int)
        (field "maxLifePB" int)
        (field "rot" int)
