module World.Decoders exposing (decoder)

import Json.Decode as Json exposing (Decoder, field, float, list, maybe, string)
import Line.Decoders as Line
import World.Types exposing (Decision, Encounter, World)


decoder : Decoder World
decoder =
    let
        posDecoder =
            Json.map2 Tuple.pair
                (Json.index 0 float)
                (Json.index 1 float)
    in
    Json.map6 World
        (field "encounters" <| list encounterDecoder)
        (field "others" <| list posDecoder)
        (field "edges" <| list Line.decoder)
        (field "visited" <| list posDecoder)
        (field "visitedEdges" <| list Line.decoder)
        (field "decision" <| maybe decisionDecoder)


encounterDecoder : Decoder Encounter
encounterDecoder =
    Json.map5 Encounter
        (field "guid" string)
        (field "name" string)
        (field "numeral" string)
        (field "x" float)
        (field "y" float)


decisionDecoder : Decoder Decision
decisionDecoder =
    Json.map5 Decision
        (field "id" string)
        (field "title" string)
        (field "text" string)
        (field "decision_choice_a_text" string)
        (field "decision_choice_b_text" string)
