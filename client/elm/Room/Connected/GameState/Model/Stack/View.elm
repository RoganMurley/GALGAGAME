module Stack.View exposing (..)

import Animation.Types exposing (Anim)
import Card.View as Card
import Html exposing (..)
import Html.Attributes exposing (..)
import Stack.Types exposing (Stack, StackCard)
import Transform exposing (Transform)
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Stack -> Maybe ( Float, Maybe Anim ) -> Html msg
view finalStack resInfo =
    let
        stack =
            finalStack

        resTick =
            Maybe.withDefault 0.0 <|
                Maybe.map Tuple.first resInfo

        viewStackCard : ( Int, StackCard ) -> Html msg
        viewStackCard ( index, { owner, card } ) =
            div
                [ classList
                    [ ( "stack-card", True )
                    , ( playerClass owner, True )
                    ]
                , style
                    [ Transform.toCss <| outerTransform index (List.length stack)
                    , ( "z-index", toString (20 - index) )
                    ]
                ]
                [ div
                    [ style [ Transform.toCss <| innerTransform index ] ]
                    [ div
                        [ classList [ ( "stack-head", index == 0 ) ] ]
                        [ Card.view card ]
                    ]
                ]
    in
        div
            [ class "stack-container" ]
            [ div
                [ class "stack" ]
                (List.map viewStackCard (List.indexedMap (,) stack))
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
        { x = x
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
        { x = 0.0
        , y = 0.0
        , r = r
        }
