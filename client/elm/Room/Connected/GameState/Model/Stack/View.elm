module Stack.View exposing (..)

import Card.View as Card
import Html exposing (..)
import Html.Attributes exposing (..)
import Stack.Types exposing (Stack, StackCard)
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Stack -> Html msg
view stack =
    let
        viewStackCard : ( Int, StackCard ) -> Html msg
        viewStackCard ( index, { owner, card } ) =
            let
                cardWidth : Float
                cardWidth =
                    14.0

                playerClass : String
                playerClass =
                    case owner of
                        PlayerA ->
                            "playera"

                        PlayerB ->
                            "playerb"

                stackLen : Float
                stackLen =
                    toFloat (List.length stack)

                offset : Int -> Float
                offset x =
                    cardWidth * (toFloat x)

                squish : Float -> Float
                squish x =
                    Basics.min 0.0 (15.0 - 0.65 * (x + 1.0) * stackLen)

                totalOffset : Float
                totalOffset =
                    (offset index)
                        - (cardWidth * stackLen * 0.5)
                        + (squish (toFloat index))
                        - ((squish (stackLen - 1.0)) * 0.5)

                rot : Float
                rot =
                    0.1 * (toFloat ((index * 1247823748932 + 142131) % 20) - 10)

                headClass : String
                headClass =
                    case index of
                        0 ->
                            " stack-head"

                        otherwise ->
                            ""
            in
                div
                    [ class (playerClass ++ " stack-card")
                    , style
                        [ ( "transform", "translateX(" ++ (toString totalOffset) ++ "rem)" )
                        , ( "z-index", toString (20 - index) )
                        ]
                    ]
                    [ div
                        [ style [ ( "transform", "rotate(" ++ (toString rot) ++ "deg)" ) ] ]
                        [ div [ class headClass ] [ Card.view card ]
                        ]
                    ]
    in
        div
            [ class "stack-container" ]
            [ div [ class "stack" ] (List.map viewStackCard (List.indexedMap (,) stack)) ]
