module Card.Decoders exposing (decoder)

import Card.Types exposing (Card, CardCol(..))
import Json.Decode as Json exposing (Decoder, fail, field, string, succeed)


decoder : Decoder Card
decoder =
    Json.map4 Card
        (field "name" string)
        (field "desc" string)
        (field "imageURL" string)
        (field "col" cardColDecoder)


cardColDecoder : Decoder CardCol
cardColDecoder =
    let
        decode : String -> Decoder CardCol
        decode s =
            case s of
                "red" ->
                    succeed Red

                "orange" ->
                    succeed Orange

                "yellow" ->
                    succeed Yellow

                "green" ->
                    succeed Green

                "blue" ->
                    succeed Blue

                "white" ->
                    succeed White

                "violet" ->
                    succeed Violet

                "mystery" ->
                    succeed Mystery

                _ ->
                    fail <| "Invalid CardCol " ++ s
    in
    string |> Json.andThen decode
