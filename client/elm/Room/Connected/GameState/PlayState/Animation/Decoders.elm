module Animation.Decoders exposing (decoder)

import Animation.Types exposing (Anim(..), Bounce(..), CardDiscard(..), Hurt(..), Transmutation(..))
import Card.Decoders as Card
import Json.Decode as Json exposing (Decoder, bool, fail, field, int, list, maybe, null, oneOf, string, succeed)
import Stack.Decoders as Stack
import Wheel.Decoders as Wheel
import WhichPlayer.Decoders as WhichPlayer


decoder : Decoder Anim
decoder =
    let
        animNameDecoder : Decoder String
        animNameDecoder =
            field "name" string

        getDecoder : String -> Decoder Anim
        getDecoder animName =
            case animName of
                "hurt" ->
                    hurtDecoder

                "heal" ->
                    healDecoder

                "draw" ->
                    drawDecoder

                "play" ->
                    playDecoder

                "transmute" ->
                    transmuteDecoder

                "mill" ->
                    millDecoder

                "gameEnd" ->
                    gameEndDecoder

                "rotate" ->
                    rotateDecoder

                "windup" ->
                    windupDecoder

                "bounce" ->
                    bounceDecoder

                "discardStack" ->
                    discardStackDecoder

                "discardHand" ->
                    discardHandDecoder

                "moveStack" ->
                    moveStackDecoder

                "pass" ->
                    passDecoder

                _ ->
                    Json.fail <| "Unknown anim name " ++ animName
    in
    oneOf
        [ animNameDecoder |> Json.andThen getDecoder
        , null NullAnim
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


hurtDecoder : Decoder Anim
hurtDecoder =
    let
        getDecoder : String -> Decoder Hurt
        getDecoder s =
            case s of
                "slash" ->
                    succeed Slash

                "bite" ->
                    succeed Bite

                "curse" ->
                    succeed Curse

                _ ->
                    fail <| s ++ " is not a valid hurt type"
    in
    Json.map3 Hurt
        (field "player" WhichPlayer.decoder)
        (field "damage" int)
        (field "hurt" string |> Json.andThen getDecoder)


healDecoder : Decoder Anim
healDecoder =
    Json.map2 Heal
        (field "player" WhichPlayer.decoder)
        (field "heal" int)


drawDecoder : Decoder Anim
drawDecoder =
    Json.map Draw
        (field "player" WhichPlayer.decoder)


playDecoder : Decoder Anim
playDecoder =
    Json.map4 Play
        (field "player" WhichPlayer.decoder)
        (field "card" Card.decoder)
        (field "index" int)
        (succeed Nothing)


transmutationDecoder : Decoder Transmutation
transmutationDecoder =
    Json.map2 Transmutation
        (field "cardA" Stack.stackCardDecoder)
        (field "cardB" Stack.stackCardDecoder)


transmuteDecoder : Decoder Anim
transmuteDecoder =
    Json.map Transmute <|
        field "transmute" <|
            Wheel.decoder <|
                maybe transmutationDecoder


millDecoder : Decoder Anim
millDecoder =
    Json.map2 Mill
        (field "player" WhichPlayer.decoder)
        (field "card" Card.decoder)


gameEndDecoder : Decoder Anim
gameEndDecoder =
    Json.map GameEnd
        (field "winner" <| Json.maybe WhichPlayer.decoder)


rotateDecoder : Decoder Anim
rotateDecoder =
    Json.map Rotate
        (field "player" WhichPlayer.decoder)


windupDecoder : Decoder Anim
windupDecoder =
    Json.map Windup
        (field "player" WhichPlayer.decoder)


bounceDecoder : Decoder Anim
bounceDecoder =
    let
        bounceDiscardDecoder : Decoder Bounce
        bounceDiscardDecoder =
            Json.map (always BounceDiscard) <| constDecoder "bounceDiscard"

        bounceIndexDecoder : Decoder Bounce
        bounceIndexDecoder =
            Json.map2 BounceIndex
                (field "stackIndex" int)
                (field "handIndex" int)
    in
    Json.map Bounce <|
        field "bounce" <|
            Wheel.decoder <|
                maybe <|
                    oneOf [ bounceDiscardDecoder, bounceIndexDecoder ]


cardDiscardDecoder : Decoder CardDiscard
cardDiscardDecoder =
    oneOf
        [ Json.map NoDiscard <| field "finalIndex" int
        , Json.map (always CardDiscard) <| constDecoder "discard"
        ]


discardStackDecoder : Decoder Anim
discardStackDecoder =
    Json.map DiscardStack <|
        field "discard" <|
            Wheel.decoder bool


discardHandDecoder : Decoder Anim
discardHandDecoder =
    Json.map2 DiscardHand
        (field "player" WhichPlayer.decoder)
        (field "discard" <| list cardDiscardDecoder)


moveStackDecoder : Decoder Anim
moveStackDecoder =
    Json.map2 MoveStack
        (field "moves" <| Wheel.decoder <| maybe int)
        (field "time" int)


passDecoder : Decoder Anim
passDecoder =
    Json.map Pass
        (field "player" WhichPlayer.decoder)
