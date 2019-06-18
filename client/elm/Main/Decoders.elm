module Main.Decoders exposing (keyDecoder)

import Json.Decode as Json exposing (Decoder)
import Keyboard exposing (Key(..))


keyDecoder : Decoder Key
keyDecoder =
    Json.field "key" Decode.string <|
        Json.andThen
            (\s ->
                case s of
                    "Enter" ->
                        Json.succeed EnterKey

                    _ ->
                        Json.fail "Unhandled key"
            )
