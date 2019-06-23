module Keyboard exposing (Key(..), keyDecoder)

import Json.Decode as Json exposing (Decoder)


type Key
    = EnterKey


keyDecoder : Decoder Key
keyDecoder =
    let
        keyFieldDecoder : String -> Decoder Key
        keyFieldDecoder string =
            case string of
                "Enter" ->
                    Json.succeed EnterKey

                _ ->
                    Json.fail "Unhandled key"
    in
    Json.field "key" Json.string |> Json.andThen keyFieldDecoder
