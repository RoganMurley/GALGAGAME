module Keyboard exposing (Key(..), keyDecoder)

import Json.Decode as Json exposing (Decoder)


type Key
    = EnterKey


keyDecoder : Decoder Key
keyDecoder =
    Json.field "key" Json.string
        |> Json.andThen
            (\s ->
                case s of
                    "Enter" ->
                        Json.succeed EnterKey

                    _ ->
                        Json.fail "Unhandled key"
            )
