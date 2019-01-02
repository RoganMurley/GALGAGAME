module Animation.Decoders exposing (decoder)

import Animation.Types exposing (Anim(..))
import Card.Decoders as Card
import Json.Decode as Json exposing (Decoder, fail, field, index, int, oneOf, string, succeed)
import Stack.Decoders as Stack
import WhichPlayer.Decoders as WhichPlayer


decoder : Decoder Anim
decoder =
    oneOf
        [ slashDecoder
        , healDecoder
        , drawDecoder
        , biteDecoder
        , reflectDecoder
        , reverseDecoder
        , hubrisDecoder
        , playDecoder
        , transmuteDecoder
        , millDecoder
        , gameEndDecoder
        , rotateDecoder
        , windupDecoder
        , fabricateDecoder
        , nullDecoder
        ]


constDecoder : String -> Decoder ()
constDecoder x =
    let
        decode : String -> Decoder ()
        decode s =
            if s == x then
                succeed ()

            else
                fail <| s ++ " does not match " ++ x
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
    Json.map3 (\w _ h -> Heal w h)
        (field "player" WhichPlayer.decoder)
        (field "anim" <| index 0 <| constDecoder "heal")
        (field "anim" <| index 1 int)


drawDecoder : Decoder Anim
drawDecoder =
    Json.map2 (\w _ -> Draw w)
        (field "player" WhichPlayer.decoder)
        (field "anim" <| constDecoder "draw")


biteDecoder : Decoder Anim
biteDecoder =
    Json.map3 (\w _ d -> Bite w d)
        (field "player" WhichPlayer.decoder)
        (field "anim" <| index 0 <| constDecoder "bite")
        (field "anim" <| index 1 int)


reflectDecoder : Decoder Anim
reflectDecoder =
    Json.map2 (\w _ -> Reflect w)
        (field "player" WhichPlayer.decoder)
        (field "anim" <| constDecoder "reflect")


reverseDecoder : Decoder Anim
reverseDecoder =
    Json.map2 (\w _ -> Reverse w)
        (field "player" WhichPlayer.decoder)
        (field "anim" <| constDecoder "reverse")


hubrisDecoder : Decoder Anim
hubrisDecoder =
    Json.map2 (\w _ -> Hubris w)
        (field "player" WhichPlayer.decoder)
        (field "anim" <| constDecoder "hubris")


playDecoder : Decoder Anim
playDecoder =
    Json.map4 (\w _ c i -> Play w c i)
        (field "player" WhichPlayer.decoder)
        (field "anim" <| index 0 <| constDecoder "play")
        (field "anim" <| index 1 <| Card.decoder)
        (field "anim" <| index 2 <| int)


transmuteDecoder : Decoder Anim
transmuteDecoder =
    Json.map4 (\w _ ca cb -> Transmute w ca cb)
        (field "player" WhichPlayer.decoder)
        (field "anim" <| index 0 <| constDecoder "transmute")
        (field "anim" <| index 1 <| Stack.stackCardDecoder)
        (field "anim" <| index 2 <| Stack.stackCardDecoder)


millDecoder : Decoder Anim
millDecoder =
    Json.map3 (\w _ c -> Mill w c)
        (field "player" WhichPlayer.decoder)
        (field "anim" <| index 0 <| constDecoder "mill")
        (field "anim" <| index 1 <| Card.decoder)


gameEndDecoder : Decoder Anim
gameEndDecoder =
    Json.map2 (\_ -> GameEnd)
        (field "anim" <| index 0 <| constDecoder "gameEnd")
        (field "anim" <| index 1 <| Json.maybe WhichPlayer.decoder)


rotateDecoder : Decoder Anim
rotateDecoder =
    Json.map2 (\w _ -> Rotate w)
        (field "player" WhichPlayer.decoder)
        (field "anim" <| constDecoder "rotate")


windupDecoder : Decoder Anim
windupDecoder =
    Json.map2 (\w _ -> Windup w)
        (field "player" WhichPlayer.decoder)
        (field "anim" <| constDecoder "windup")


fabricateDecoder : Decoder Anim
fabricateDecoder =
    Json.map3 (\_ _ c -> Fabricate c)
        (field "player" WhichPlayer.decoder)
        (field "anim" <| index 0 <| constDecoder "fabricate")
        (field "anim" <| index 1 <| Stack.stackCardDecoder)


nullDecoder : Decoder Anim
nullDecoder =
    Json.succeed NullAnim
