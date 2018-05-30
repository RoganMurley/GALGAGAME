module Stack.View exposing (..)

import Animation.State exposing (animToResTickMax)
import Animation.Types exposing (Anim(Play, Reverse, Transmute))
import Card.View as Card
import Ease
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe.Extra as Maybe
import Stack.Types exposing (Stack, StackAnim(..), StackCard)
import Transform exposing (Transform, origin)
import Util exposing (maybeCons)
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Stack -> Maybe StackCard -> Maybe ( Float, Maybe Anim ) -> Html msg
view finalStack stackCard resInfo =
    let
        stackAnim =
            case resInfo of
                Just ( _, Just (Play _ _ _) ) ->
                    Just Playing

                Just ( _, Just (Reverse _) ) ->
                    Just Reversing

                Just ( _, Just (Transmute _ ca cb) ) ->
                    Just (Transmuting ca cb)

                otherwise ->
                    Nothing

        stack =
            maybeCons stackCard finalStack

        resTick =
            Maybe.withDefault 0.0 <|
                Maybe.map Tuple.first resInfo

        anim =
            Maybe.map Tuple.second resInfo

        resTickMax =
            animToResTickMax (Maybe.join anim)

        stackLen =
            List.length stack

        hasStackCard =
            Maybe.isJust stackCard

        viewStackCard : ( Int, StackCard ) -> Html msg
        viewStackCard ( finalIndex, finalCard ) =
            let
                index =
                    case stackAnim of
                        Just Playing ->
                            if finalIndex == 0 then
                                0
                            else
                                finalIndex - 1

                        Just Reversing ->
                            if finalIndex == 0 then
                                0
                            else
                                stackLen - finalIndex

                        otherwise ->
                            finalIndex

                { owner, card } =
                    case ( index, stackAnim ) of
                        ( 1, Just (Transmuting initialCard _) ) ->
                            if resTick < (resTickMax * 0.5) then
                                initialCard
                            else
                                finalCard

                        otherwise ->
                            finalCard

                outerEase =
                    case stackAnim of
                        Just Playing ->
                            Ease.outQuad

                        Just Reversing ->
                            Ease.inOutCirc

                        Just (Transmuting _ _) ->
                            Ease.outQuad

                        otherwise ->
                            identity

                outerSkew : Transform -> Transform
                outerSkew trans =
                    case ( index, stackAnim ) of
                        ( 1, Just (Transmuting _ _) ) ->
                            { trans | sx = 180 }

                        otherwise ->
                            trans
            in
                div
                    [ classList
                        [ ( "stack-card", True )
                        , ( playerClass owner, True )
                        ]
                    , style
                        [ Transform.toCss <|
                            Transform.ease outerEase
                                (resTick / resTickMax)
                                (outerSkew <| outerTransform index stackLen)
                                (outerTransform finalIndex stackLen)
                        , ( "z-index", toString (20 - finalIndex) )
                        ]
                    ]
                    [ div
                        [ style
                            [ Transform.toCss <|
                                Transform.ease Ease.outQuint
                                    (resTick / resTickMax)
                                    (innerTransform index)
                                    (innerTransform finalIndex)
                            ]
                        ]
                        [ div
                            [ classList [ ( "stack-head", index == 0 && hasStackCard ) ] ]
                            [ Card.view card ]
                        ]
                    ]
    in
        div
            [ class "stack-container" ]
            [ div
                [ class "stack" ]
                (List.map viewStackCard <| List.indexedMap (,) stack)
            ]


playerClass : WhichPlayer -> String
playerClass which =
    case which of
        PlayerA ->
            "playera"

        PlayerB ->
            "playerb"


outerTransform : Int -> Int -> Transform
outerTransform index stackLen =
    let
        cardWidth : Float
        cardWidth =
            14.0

        offset : Float -> Float
        offset x =
            cardWidth * x

        squish : Float -> Float
        squish x =
            Basics.min 0.0 (15.0 - 0.65 * (x + 1.0) * (toFloat stackLen))

        x : Float
        x =
            (offset <| toFloat index)
                - (cardWidth * (toFloat stackLen) * 0.5)
                + (squish (toFloat index))
                - ((squish ((toFloat stackLen) - 1.0)) * 0.5)
    in
        { origin
            | x = x
            , y = 0.0
            , r = 0.0
        }


innerTransform : Int -> Transform
innerTransform index =
    let
        -- Pseudrandom
        r =
            0.1 * (toFloat ((index * 1247823748932 + 142131) % 20) - 10)
    in
        { origin
            | x = 0.0
            , y = 0.0
            , r = r
        }
