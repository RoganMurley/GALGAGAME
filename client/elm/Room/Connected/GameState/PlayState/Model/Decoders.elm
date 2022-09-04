module Model.Decoders exposing (decoder)

import Card.Decoders as Card
import Json.Decode as Json exposing (Decoder, fail, field, int, list, maybe, string, succeed)
import Model.Types exposing (Model, Pass(..))
import Stack.Decoders as Stack
import WhichPlayer.Decoders as WhichPlayer


decoder : Decoder Model
decoder =
    Json.map8 (<|)
        (Json.map5 Model
            (field "handPA" <| list Card.knowableCardDecoder)
            (field "handPB" <| list (maybe Card.decoder))
            (field "deckPA" int)
            (field "deckPB" int)
            (field "stack" Stack.decoder)
        )
        (field "turn" WhichPlayer.decoder)
        (field "lifePA" int)
        (field "lifePB" int)
        (field "maxLifePA" int)
        (field "maxLifePB" int)
        (field "rot" int)
        (field "pass" passDecoder)


passDecoder : Decoder Pass
passDecoder =
    Json.andThen
        (\str ->
            case str of
                "NoPass" ->
                    succeed NoPass

                "OnePass" ->
                    succeed OnePass

                _ ->
                    fail <| "unknown pass value " ++ str
        )
        string
