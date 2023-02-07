module Animation.State exposing (animMaxTick, animShake, getPlayerBounceCards, getPlayerBounceDeckCards, progress)

import Animation.Types exposing (Anim(..), Bounce(..), DeckBounce, HandBounce)
import Card.Types exposing (Card)
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
    1.2
        * (case anim of
            Draw _ timeModifier ->
                timeModifier.mod * 250.0

            Play _ _ _ _ ->
                300.0

            Mill _ _ timeModifier ->
                timeModifier.mod * 500.0

            GameEnd _ ->
                1250.0

            Rotate _ ->
                400.0

            Windup _ ->
                400.0

            Bounce _ timeModifier ->
                timeModifier.mod * 750.0

            BounceDeck _ timeModifier ->
                timeModifier.mod * 750.0

            DiscardStack _ ->
                750.0

            DiscardHand _ _ ->
                400.0

            Hurt _ _ _ ->
                400.0

            Heal _ _ ->
                400.0

            MoveStack _ timeModifier ->
                timeModifier.mod * 1.0

            GetGen ->
                1

            Timeout ->
                2000

            Announce _ timeModifier ->
                timeModifier.mod * 400

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
                Draw _ timeModifier ->
                    timeModifier.ease

                Bounce _ timeModifier ->
                    timeModifier.ease

                BounceDeck _ timeModifier ->
                    Ease.outQuad << timeModifier.ease

                Hurt _ _ _ ->
                    Ease.outQuint

                Heal _ _ ->
                    Ease.outQuint

                Mill _ _ timingModifier ->
                    timingModifier.ease

                MoveStack _ timeModifier ->
                    timeModifier.ease

                Rotate _ ->
                    Ease.inQuad

                Windup _ ->
                    Ease.outBounce

                Announce _ _ ->
                    Ease.outExpo

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


getPlayerBounceDeckCards : WhichPlayer -> Wheel Bool -> Stack -> List DeckBounce
getPlayerBounceDeckCards w bounces stack =
    let
        makeBounce : ( Bool, Maybe StackCard ) -> Maybe Card
        makeBounce input =
            case input of
                ( True, Just { owner, card } ) ->
                    if owner == w then
                        Just card

                    else
                        Nothing

                _ ->
                    Nothing

        addStackIndex : Int -> Maybe Card -> Maybe DeckBounce
        addStackIndex i =
            Maybe.map (\card -> { card = card, stackIndex = i })
    in
    List.filterMap identity <|
        Wheel.toList <|
            Wheel.indexedMap addStackIndex <|
                Wheel.map makeBounce <|
                    Wheel.apply
                        (Wheel.map (\x y -> ( x, y )) bounces)
                        stack
