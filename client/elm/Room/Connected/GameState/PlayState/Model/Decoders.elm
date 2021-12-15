module Model.Decoders exposing (decoder)

import Card.Decoders as Card
import Json.Decode as Json exposing (Decoder, field, int, list, maybe)
import Model.Types exposing (Model)
import Stack.Decoders as Stack
import WhichPlayer.Decoders as WhichPlayer


decoder : Decoder Model
decoder =
    Json.map8 (<|)
        (Json.map4 Model
            (field "handPA" <| list Card.decoder)
            (field "handPB" <| list (maybe Card.decoder))
            (field "deckPA" int)
            (field "deckPB" int)
        )
        (field "stack" Stack.decoder)
        (field "turn" WhichPlayer.decoder)
        (field "lifePA" int)
        (field "lifePB" int)
        (field "maxLifePA" int)
        (field "maxLifePB" int)
        (field "rot" int)
