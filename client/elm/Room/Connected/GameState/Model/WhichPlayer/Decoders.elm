module WhichPlayer.Decoders exposing (..)

import Json.Decode as Json exposing (Decoder, fail, string, succeed)
import WhichPlayer.Types exposing (WhichPlayer(..))


decoder : Decoder WhichPlayer
decoder =
    let
        decode : String -> Decoder WhichPlayer
        decode s =
            case s of
                "pa" ->
                    succeed PlayerA

                "pb" ->
                    succeed PlayerB

                _ ->
                    fail ("Invalid WhichPlayer " ++ s)
    in
        string |> Json.andThen decode
