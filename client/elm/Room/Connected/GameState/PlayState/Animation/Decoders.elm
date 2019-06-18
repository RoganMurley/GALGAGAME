module Animation.Decoders exposing (decoder)

import Animation.Types exposing (Anim(..), Bounce(..))
import Card.Decoders as Card
import Json.Decode as Json exposing (Decoder, fail, field, int, list, null, oneOf, string, succeed)
import Stack.Decoders as Stack
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
                "slash" ->
                    slashDecoder

                "heal" ->
                    healDecoder

                "curse" ->
                    curseDecoder

                "draw" ->
                    drawDecoder

                "bite" ->
                    biteDecoder

                "reflect" ->
                    reflectDecoder

                "reverse" ->
                    reverseDecoder

                "confound" ->
                    confoundDecoder

                "hubris" ->
                    hubrisDecoder

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

                "fabricate" ->
                    fabricateDecoder

                "bounce" ->
                    bounceDecoder

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


slashDecoder : Decoder Anim
slashDecoder =
    Json.map2 Slash
        (field "player" WhichPlayer.decoder)
        (field "damage" int)


healDecoder : Decoder Anim
healDecoder =
    Json.map2 Heal
        (field "player" WhichPlayer.decoder)
        (field "heal" int)


drawDecoder : Decoder Anim
drawDecoder =
    Json.map Draw
        (field "player" WhichPlayer.decoder)


biteDecoder : Decoder Anim
biteDecoder =
    Json.map2 Bite
        (field "player" WhichPlayer.decoder)
        (field "damage" int)


curseDecoder : Decoder Anim
curseDecoder =
    Json.map2 Curse
        (field "player" WhichPlayer.decoder)
        (field "damage" int)


reflectDecoder : Decoder Anim
reflectDecoder =
    Json.map Reflect
        (field "player" WhichPlayer.decoder)


confoundDecoder : Decoder Anim
confoundDecoder =
    Json.map Confound
        (field "player" WhichPlayer.decoder)


reverseDecoder : Decoder Anim
reverseDecoder =
    Json.map Reverse
        (field "player" WhichPlayer.decoder)


hubrisDecoder : Decoder Anim
hubrisDecoder =
    Json.map Hubris
        (field "player" WhichPlayer.decoder)


playDecoder : Decoder Anim
playDecoder =
    Json.map3 Play
        (field "player" WhichPlayer.decoder)
        (field "card" Card.decoder)
        (field "index" int)


transmuteDecoder : Decoder Anim
transmuteDecoder =
    Json.map3 Transmute
        (field "player" WhichPlayer.decoder)
        (field "cardA" Stack.stackCardDecoder)
        (field "cardB" Stack.stackCardDecoder)


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


fabricateDecoder : Decoder Anim
fabricateDecoder =
    Json.map Fabricate
        (field "stackCard" Stack.stackCardDecoder)


bounceDecoder : Decoder Anim
bounceDecoder =
    let
        noBounceDecoder : Decoder Bounce
        noBounceDecoder =
            Json.map NoBounce <| field "finalStackIndex" int

        bounceDiscardDecoder : Decoder Bounce
        bounceDiscardDecoder =
            Json.map (always BounceDiscard) <| constDecoder "bounceDiscard"

        bounceIndexDecoder : Decoder Bounce
        bounceIndexDecoder =
            Json.map2 BounceIndex
                (field "stackIndex" int)
                (field "handIndex" int)
    in
    Json.map Bounce
        (field "bounce" <|
            list <|
                oneOf [ noBounceDecoder, bounceDiscardDecoder, bounceIndexDecoder ]
        )


passDecoder : Decoder Anim
passDecoder =
    Json.map Pass
        (field "player" WhichPlayer.decoder)
