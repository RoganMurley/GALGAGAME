module Animation.State exposing (animMaxTick, animShake, getPlayerBounceCards, lifeChange, progress)

import Animation.Types exposing (Anim(..), Bounce(..), HandBounce)
import Ease
import Stack.Types exposing (Stack, StackCard)
import Util exposing (zip)
import WhichPlayer.Types exposing (WhichPlayer(..))


animShake : Anim -> WhichPlayer -> Float -> Float
animShake anim which tick =
    let
        baseMag =
            case anim of
                Slash w d ->
                    if which /= w then
                        0

                    else
                        1.0 * Ease.outQuad (toFloat d / 50.0)

                Bite w d ->
                    if which /= w then
                        0

                    else
                        1.0 * Ease.outQuad (toFloat d / 50.0)

                _ ->
                    0.0

        mag =
            baseMag * (1.0 - Ease.outQuad (tick / animMaxTick anim))
    in
    mag * (toFloat <| (ceiling tick * 1247823748932 + 142131) % 20) - 10


animMaxTick : Anim -> Float
animMaxTick anim =
    case anim of
        Draw _ ->
            500.0

        Reverse _ ->
            1500.0

        Play _ _ _ ->
            500.0

        Mill _ _ ->
            1000.0

        Hubris _ ->
            1000.0

        GameEnd _ ->
            2500.0

        Rotate _ ->
            1000.0

        Windup _ ->
            300.0

        Bounce _ ->
            1500.0

        _ ->
            800.0


progress : Anim -> Float -> Float
progress anim tick =
    let
        maxTick : Float
        maxTick =
            animMaxTick anim

        easingFunction : Float -> Float
        easingFunction =
            case anim of
                Heal _ _ ->
                    Ease.outQuad

                Mill _ _ ->
                    Ease.outQuint

                Rotate _ ->
                    Ease.inQuad

                Slash _ _ ->
                    Ease.outQuad

                Bite _ _ ->
                    Ease.outQuad

                Windup _ ->
                    Ease.inQuad

                _ ->
                    Ease.outQuint
    in
    easingFunction (tick / maxTick)


lifeChange : Anim -> ( Float, Float )
lifeChange anim =
    let
        wrap : WhichPlayer -> Int -> ( Float, Float )
        wrap w d =
            case w of
                PlayerA ->
                    ( toFloat d, 0 )

                PlayerB ->
                    ( 0, toFloat d )
    in
    case anim of
        Heal w h ->
            wrap w h

        Slash w d ->
            wrap w -d

        Bite w d ->
            wrap w -d

        _ ->
            ( 0, 0 )


getPlayerBounceCards : WhichPlayer -> List Bounce -> Stack -> List HandBounce
getPlayerBounceCards w bounces stack =
    let
        makeBounce : ( Bounce, StackCard ) -> Maybe HandBounce
        makeBounce ( bounce, { owner, card } ) =
            if owner == w then
                case bounce of
                    BounceIndex stackIndex handIndex ->
                        Just
                            { stackIndex = stackIndex
                            , handIndex = handIndex
                            , card = card
                            }

                    NoBounce _ ->
                        Nothing

                    BounceDiscard ->
                        Nothing

            else
                Nothing
    in
    List.filterMap identity <|
        List.map makeBounce <|
            zip bounces stack
