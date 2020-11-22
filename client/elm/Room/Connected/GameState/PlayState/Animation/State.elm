module Animation.State exposing (animMaxTick, animShake, getPlayerBounceCards, progress)

import Animation.Types exposing (Anim(..), Bounce(..), HandBounce)
import Ease
import Stack.Types exposing (Stack, StackCard)
import Wheel.State as Wheel
import Wheel.Types exposing (Wheel)
import WhichPlayer.Types exposing (WhichPlayer(..))


animShake : Anim -> WhichPlayer -> Float -> Float
animShake anim which tick =
    let
        baseMag =
            case anim of
                Hurt w d _ ->
                    if which /= w then
                        0

                    else
                        Ease.outQuad (toFloat d / 50.0)

                _ ->
                    0.0

        mag =
            baseMag * (1.0 - Ease.outQuad (tick / animMaxTick anim))
    in
    mag * (toFloat <| modBy 20 (ceiling tick * 1247823748932 + 142131))


animMaxTick : Anim -> Float
animMaxTick anim =
    1.5
        * (case anim of
            Draw _ ->
                250.0

            Play _ _ _ _ ->
                300.0

            Mill _ _ ->
                500.0

            GameEnd _ ->
                1250.0

            Rotate _ ->
                400.0

            Windup _ ->
                400.0

            Bounce _ ->
                750.0

            DiscardStack _ ->
                750.0

            DiscardHand _ _ ->
                400.0

            Hurt _ _ _ ->
                400.0

            Heal _ _ ->
                400.0

            MoveStack _ time ->
                toFloat time

            _ ->
                400.0
          )


progress : Anim -> Float -> Float
progress anim tick =
    let
        maxTick : Float
        maxTick =
            animMaxTick anim

        easingFunction : Float -> Float
        easingFunction =
            case anim of
                Hurt _ _ _ ->
                    Ease.outQuint

                Heal _ _ ->
                    Ease.outQuint

                Mill _ _ ->
                    Ease.outQuint

                Rotate _ ->
                    Ease.inQuad

                Windup _ ->
                    Ease.outBounce

                _ ->
                    Ease.outQuint
    in
    easingFunction (tick / maxTick)


getPlayerBounceCards : WhichPlayer -> Wheel (Maybe Bounce) -> Stack -> List HandBounce
getPlayerBounceCards w bounces stack =
    let
        makeBounce : ( Maybe Bounce, Maybe StackCard ) -> Maybe HandBounce
        makeBounce input =
            case input of
                ( Just bounce, Just { owner, card } ) ->
                    if owner == w then
                        case bounce of
                            BounceIndex stackIndex handIndex ->
                                Just
                                    { stackIndex = stackIndex
                                    , handIndex = handIndex
                                    , card = card
                                    }

                            BounceDiscard ->
                                Nothing

                    else
                        Nothing

                _ ->
                    Nothing
    in
    List.filterMap identity <|
        Wheel.toList <|
            Wheel.map makeBounce <|
                Wheel.apply
                    (Wheel.map (\x y -> ( x, y )) bounces)
                    stack
