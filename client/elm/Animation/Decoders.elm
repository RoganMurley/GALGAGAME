module Animation.Decoders exposing (decoder)

import Animation.Types exposing (Anim(..))
import Card.Decoders as Card
import Json.Decode as Json exposing (Decoder, fail, field, index, int, oneOf, string, succeed)
import WhichPlayer.Decoders as WhichPlayer


decoder : Decoder Anim
decoder =
    oneOf
        [ slashDecoder
        , healDecoder
        , drawDecoder
        , reverseDecoder
        , obliterateDecoder
        , playDecoder
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
    Json.map3 (\w _ d -> Slash w d)
        (field "player" WhichPlayer.decoder)
        (field "anim" <| index 0 <| constDecoder "slash")
        (field "anim" <| index 1 int)


healDecoder : Decoder Anim
healDecoder =
    Json.map2 (\w _ -> Heal w)
        (field "player" WhichPlayer.decoder)
        (field "anim" (constDecoder "heal"))


drawDecoder : Decoder Anim
drawDecoder =
    Json.map2 (\w _ -> Draw w)
        (field "player" WhichPlayer.decoder)
        (field "anim" (constDecoder "draw"))


reverseDecoder : Decoder Anim
reverseDecoder =
    Json.map2 (\w _ -> Reverse w)
        (field "player" WhichPlayer.decoder)
        (field "anim" (constDecoder "reverse"))


obliterateDecoder : Decoder Anim
obliterateDecoder =
    Json.map2 (\w _ -> Obliterate w)
        (field "player" WhichPlayer.decoder)
        (field "anim" (constDecoder "obliterate"))


playDecoder : Decoder Anim
playDecoder =
    Json.map3 (\w _ c -> Play w c)
        (field "player" WhichPlayer.decoder)
        (field "anim" <| index 0 <| constDecoder "play")
        (field "anim" <| index 1 <| Card.decoder)
