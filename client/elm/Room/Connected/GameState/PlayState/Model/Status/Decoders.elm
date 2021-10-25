module Status.Decoders exposing (decoder)

import Json.Decode as Json exposing (Decoder, string)
import Status.Types exposing (Status(..))


decoder : Decoder Status
decoder =
    let
        getDecoder : String -> Decoder Status
        getDecoder str =
            case str of
                "StatusBlighted" ->
                    Json.succeed StatusBlighted

                "StatusEcho" ->
                    Json.succeed StatusEcho

                "StatusNegate" ->
                    Json.succeed StatusNegate

                "StatusVast" ->
                    Json.succeed StatusVast

                _ ->
                    Json.fail <| "Unknown status " ++ str
    in
    string |> Json.andThen getDecoder
