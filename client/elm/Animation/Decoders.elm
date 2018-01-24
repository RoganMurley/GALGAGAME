module Animation.Decoders exposing (decoder)

import Animation.Types exposing (Anim(..))
import Json.Decode as Json exposing (Decoder, fail, field, oneOf, string, succeed)
import Model.Decoders exposing (whichDecoder)


decoder : Decoder Anim
decoder =
    oneOf
        [ slashDecoder
        , healDecoder
        , obliterateDecoder
        , drawDecoder
        ]


constDecoder : String -> Decoder ()
constDecoder x =
    let
        decode : String -> Decoder ()
        decode s =
            if s == x then
                succeed ()
            else
                fail (s ++ " does not match " ++ x)
    in
        string |> Json.andThen decode


slashDecoder : Decoder Anim
slashDecoder =
    Json.map2 (\w _ -> Slash w)
        (field "player" whichDecoder)
        (field "anim" (constDecoder "slash"))


healDecoder : Decoder Anim
healDecoder =
    Json.map2 (\w _ -> Heal w)
        (field "player" whichDecoder)
        (field "anim" (constDecoder "heal"))


obliterateDecoder : Decoder Anim
obliterateDecoder =
    Json.map2 (\w _ -> Obliterate w)
        (field "player" whichDecoder)
        (field "anim" (constDecoder "obliterate"))


drawDecoder : Decoder Anim
drawDecoder =
    Json.map2 (\w _ -> Draw w)
        (field "player" whichDecoder)
        (field "anim" (constDecoder "draw"))
