module Model.View exposing (..)

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
import Stack.Types exposing (..)
import Stack.View as Stack
import WhichPlayer.Types exposing (WhichPlayer(..))


playingOnly : PlayingOnly -> Main.Msg
playingOnly =
    Main.RoomMsg
        << Room.ConnectedMsg
        << Connected.GameStateMsg
        << PlayingOnly


view : ( Model, ViewModel ) -> Float -> Html Main.Msg
view ( model, viewModel ) time =
    div [ class "game-container", style [ screenshakeStyle viewModel.shake time ] ]
        [ viewOtherHand model.otherHand model.otherHover 0 Nothing
        , Html.map playingOnly <|
            viewHand model.hand viewModel.hover 0 False Nothing
        , Stack.view model.stack
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



-- RESOLVING VIEW.


resView : Model.ViewModel.ViewModel -> Resolvable.ResolveData -> Float -> Float -> Html Main.Msg
resView vm { model, stackCard, anim } time resTick =
    let
        ( stack, hasStackCardClass ) =
            case stackCard of
                Just c ->
                    ( c :: model.stack, "hasStackCard" )

                Nothing ->
                    ( model.stack, "" )
    in
        div
            [ class ("game-container resolving " ++ hasStackCardClass)
            , style [ screenshakeStyle vm.shake time ]
            ]
            [ viewOtherHand model.otherHand model.otherHover resTick anim
            , Html.map playingOnly <|
                viewHand model.hand vm.hover resTick True anim
            , Stack.view stack
            , viewResTurn stackCard
            , viewStatus PlayerA model.life
            , viewStatus PlayerB model.otherLife
            ]


viewResTurn : Maybe StackCard -> Html msg
viewResTurn stackCard =
    div
        [ class "turn-indi" ]
        [ case stackCard of
            Just _ ->
                text "Resolving..."

            Nothing ->
                text ""
        ]



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
