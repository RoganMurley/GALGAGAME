module Card.Decoders exposing (decoder, knowableCardDecoder)

import Card.Types exposing (Card, KnowableCard(..), Related)
import Json.Decode as Json exposing (Decoder, bool, field, list, string)
import Status.Decoders as Status


decoder : Decoder Card
decoder =
    Json.map5 Card
        (field "name" string)
        (field "desc" string)
        (field "imageURL" string)
        (field "statuses" <| list Status.decoder)
        (field "related" <| list relatedDecoder)


relatedDecoder : Decoder Related
relatedDecoder =
    Json.map3 Related
        (field "name" string)
        (field "desc" string)
        (field "imageURL" string)


knowableCardDecoder : Decoder KnowableCard
knowableCardDecoder =
    let
        getDecoder : Bool -> Decoder KnowableCard
        getDecoder known =
            Json.map
                (if known then
                    KnownCard

                 else
                    UnknownCard
                )
                (field "card" decoder)
    in
    field "known" bool |> Json.andThen getDecoder
