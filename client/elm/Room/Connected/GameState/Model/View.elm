module Model.View exposing (..)

import Card.View as Card
import Connected.Messages as Connected
import Hand.State exposing (maxHandLength)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Hand.View exposing (viewHand, viewOtherHand)
import Main.Messages as Main
import Resolvable.Types as Resolvable
import Room.Messages as Room
import GameState.Messages exposing (..)
import Model.Types exposing (..)
import Model.ViewModel exposing (..)


playingOnly : PlayingOnly -> Main.Msg
playingOnly =
    Main.RoomMsg
        << Room.ConnectedMsg
        << Connected.GameStateMsg
        << PlayingOnly


view : ( Model, ViewModel ) -> Float -> Html Main.Msg
view ( model, viewModel ) time =
    div [ class "game-container", style [ screenshakeStyle viewModel.shake time ] ]
        [ viewOtherHand model.otherHand model.otherHover
        , Html.map playingOnly <|
            viewHand model.hand viewModel.hover 0 False Nothing
        , viewStack model.stack
        , Html.map playingOnly <|
            viewTurn (List.length model.hand == maxHandLength) model.turn
        , viewStatus PlayerA model.life
        , viewStatus PlayerB model.otherLife
        ]


viewTurn : Bool -> WhichPlayer -> Html PlayingOnly
viewTurn handFull turn =
    case turn of
        PlayerA ->
            case handFull of
                False ->
                    button
                        [ class "turn-indi pass-button"
                        , onClick <|
                            TurnOnly <|
                                EndTurn
                        ]
                        [ text "Pass" ]

                True ->
                    button
                        [ class "turn-indi pass-button pass-disabled" ]
                        [ text "Hand full" ]

        PlayerB ->
            div
                [ class "turn-indi enemy-turn" ]
                [ text "Opponent's Turn" ]


viewStatus : WhichPlayer -> Life -> Html msg
viewStatus which life =
    div
        [ classList
            [ ( "status", True )
            , ( "status-mine", which == PlayerA )
            ]
        ]
        [ viewLife life
        ]


viewLife : Life -> Html msg
viewLife life =
    let
        barWidth : Life -> String
        barWidth barLife =
            (toString (((toFloat barLife) / 50) * 100)) ++ "%"
    in
        div
            [ class "life" ]
            [ div
                [ class "life-bar" ]
                [ div [ class "life-text" ] [ text ("♥ " ++ (toString life) ++ " ♥") ]
                , div [ class "life-health", style [ ( "width", barWidth life ) ] ] []
                ]
            ]


viewStack : Stack -> Html msg
viewStack stack =
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



-- RESOLVING VIEW.


resView : Model.ViewModel.ViewModel -> Resolvable.ResolveData -> Float -> Float -> Html Main.Msg
resView vm { model, stackCard, anim } time resTick =
    let
        stack : List StackCard
        stack =
            stackCard :: model.stack
    in
        div [ class "game-container resolving", style [ screenshakeStyle vm.shake time ] ]
            [ viewOtherHand model.otherHand model.otherHover
            , Html.map playingOnly <|
                viewHand model.hand vm.hover resTick True anim
            , viewStack stack
            , viewResTurn
            , viewStatus PlayerA model.life
            , viewStatus PlayerB model.otherLife
            ]


viewResTurn : Html msg
viewResTurn =
    div
        [ class "turn-indi" ]
        [ text "Resolving..." ]



-- SCREENSHAKE


screenshakeStyle : Float -> Float -> ( String, String )
screenshakeStyle shake time =
    let
        x : Float
        x =
            shake * 0.3 * (toFloat (((ceiling time) * 1247823748932 + 142131) % 20) - 10)

        y : Float
        y =
            shake * 0.3 * (toFloat (((ceiling time) * 1247823748932 + 142131) % 20) - 10)
    in
        ( "transform"
        , "translate(" ++ (toString x) ++ "px, " ++ (toString y) ++ "px)"
        )
