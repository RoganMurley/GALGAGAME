module World.Decoders exposing (decoder)

import Card.Decoders as Card
import Json.Decode as Json exposing (Decoder, fail, field, float, list, maybe, string, succeed)
import Line.Decoders as Line
import World.Types exposing (Decision, DecisionChoice, Encounter, Variant(..), World)


decoder : Decoder World
decoder =
    let
        posDecoder =
            Json.map2 Tuple.pair
                (Json.index 0 float)
                (Json.index 1 float)
    in
    Json.map7 World
        (field "encounters" <| list encounterDecoder)
        (field "others" <| list posDecoder)
        (field "edges" <| list Line.decoder)
        (field "visited" <| list posDecoder)
        (field "visitedEdges" <| list Line.decoder)
        (field "lockedEdges" <| list Line.decoder)
        (field "decision" <| maybe decisionDecoder)


encounterDecoder : Decoder Encounter
encounterDecoder =
    Json.map4 Encounter
        (field "guid" string)
        (field "x" float)
        (field "y" float)
        (field "variant" variantDecoder)


decisionDecoder : Decoder Decision
decisionDecoder =
    Json.map5 Decision
        (field "id" string)
        (field "title" string)
        (field "text" string)
        (field "cards" <| list Card.decoder)
        (field "choices" <| list decisionChoiceDecoder)


decisionChoiceDecoder : Decoder DecisionChoice
decisionChoiceDecoder =
    Json.map DecisionChoice
        (field "text" string)


variantDecoder : Decoder Variant
variantDecoder =
    string
        |> Json.andThen
            (\var ->
                case var of
                    "cpu" ->
                        succeed CpuVariant

                    "pvp" ->
                        succeed PvpVariant

                    _ ->
                        fail <| "Unknown encounter variant " ++ var
            )
