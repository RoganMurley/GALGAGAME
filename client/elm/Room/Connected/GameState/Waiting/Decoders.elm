module Waiting.Decoders exposing (decoder)

import Json.Decode as Json exposing (Decoder, fail, field, string, succeed)
import Waiting.State exposing (init)
import Waiting.Types exposing (Model, WaitType(..))


decoder : Decoder Model
decoder =
    Json.map init
        (field "waiting" (string |> Json.andThen waitTypeDecoder))


waitTypeDecoder : String -> Decoder (Maybe WaitType)
waitTypeDecoder s =
    case s of
        "quickplay" ->
            succeed <| Just WaitQuickplay

        "custom" ->
            succeed <| Just WaitCustom

        "challenge" ->
            succeed <| Just <| WaitChallenge 0

        _ ->
            fail <| "Invalid WaitType " ++ s
