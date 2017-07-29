module Model.View exposing (..)

import Card.Types exposing (Card)
import Card.View as Card
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Main.Messages exposing (Msg(..))
import Model.Types exposing (..)
import Model.State exposing (maxHandLength)


cardWidth : Float
cardWidth =
    14.0


view : Int -> Model -> Html Msg
view resTime model =
    div []
        [ viewOtherHand model.otherHand model.otherHover
        , viewHand model.hand False
        , viewStack model.stack
        , viewTurn (List.length model.hand == maxHandLength) model.turn
        , viewStatus PlayerA model.life
        , viewStatus PlayerB model.otherLife
        ]


viewHand : Hand -> Bool -> Html Msg
viewHand hand resolving =
    let
        mouseActions : Int -> List (Attribute Msg)
        mouseActions index =
            if resolving then
                []
            else
                [ onClick <| PlayCard index
                , onMouseEnter <| HoverCard <| Just index
                , onMouseLeave <| HoverCard Nothing
                ]

        calcRot : Int -> Float
        calcRot index =
            -1.5 * ((toFloat index) - (cardCount * 0.5))

        calcTrans : Int -> Float
        calcTrans index =
            -12.0 * ((toFloat index) - (cardCount * 0.5))

        cardCount : Float
        cardCount =
            toFloat <| List.length hand

        cardView : ( Int, Card ) -> Html Msg
        cardView ( index, { name, desc, imgURL } ) =
            div
                [ class "my-card-container"
                , style
                    [ ( "transform"
                      , "translateX("
                            ++ (toString <| calcTrans index)
                            ++ "rem) rotate("
                            ++ (toString <| calcRot index)
                            ++ "deg)"
                      )
                    ]
                ]
                [ div
                    ([ class "card my-card"
                     ]
                        ++ (mouseActions index)
                    )
                    [ div [ class "card-title" ] [ text name ]
                    , div
                        [ class "card-picture"
                        , style [ ( "background-image", "url(\"/img/" ++ imgURL ++ "\")" ) ]
                        ]
                        []
                    , div [ class "card-desc" ] [ text desc ]
                    ]
                ]
    in
        div
            [ class "hand my-hand" ]
            (List.map cardView (List.indexedMap (,) hand))


viewOtherHand : Int -> HoverCardIndex -> Html Msg
viewOtherHand cardCountInt hoverIndex =
    let
        cardCount : Float
        cardCount =
            toFloat cardCountInt

        cardView : Int -> Html Msg
        cardView index =
            div [ containerClass index hoverIndex ]
                [ div
                    [ class "card other-card"
                      -- , style [ ( "transform", "rotateZ(" ++ toString (calcRot index) ++ "deg) translateY(" ++ toString (calcTrans index) ++ "px)" ) ]
                    , style
                        [ ( "transform"
                          , "translateX("
                                ++ (toString <| calcTrans index)
                                ++ "rem) rotate("
                                ++ (toString <| calcRot index)
                                ++ "deg)"
                          )
                        ]
                    ]
                    []
                ]

        -- Stupid container nesting because css transform overwrite.
        containerClass : Int -> HoverCardIndex -> Attribute msg
        containerClass index hoverIndex =
            case hoverIndex of
                Just i ->
                    if i == index then
                        class "other-card-container card-hover"
                    else
                        class "other-card-container"

                Nothing ->
                    class "other-card-container"

        cards : List (Html Msg)
        cards =
            List.map cardView (List.range 0 (cardCountInt - 1))

        calcRot : Int -> Float
        calcRot index =
            1.5 * ((toFloat index) - (cardCount * 0.5))

        calcTrans : Int -> Float
        calcTrans index =
            -12.0 * ((toFloat index) - (cardCount * 0.5))
    in
        div [ class "hand other-hand" ] cards


viewTurn : Bool -> WhichPlayer -> Html Msg
viewTurn handFull turn =
    case turn of
        PlayerA ->
            case handFull of
                False ->
                    button
                        [ class "turn-indi pass-button", onClick EndTurn ]
                        [ text "Pass" ]

                True ->
                    button
                        [ class "turn-indi pass-button pass-disabled" ]
                        [ text "Hand full" ]

        PlayerB ->
            div
                [ class "turn-indi enemy-turn" ]
                [ text "Opponent's Turn" ]


viewStatus : WhichPlayer -> Life -> Html Msg
viewStatus which life =
    div
        [ classList
            [ ( "status", True )
            , ( "status-mine", which == PlayerA )
            ]
        ]
        [ viewLife life
        ]


viewLife : Life -> Html Msg
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


viewStack : Stack -> Html Msg
viewStack stack =
    let
        viewStackCard : ( Int, StackCard ) -> Html Msg
        viewStackCard ( index, { owner, card } ) =
            let
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
                        [ div
                            [ class headClass ]
                            [ Card.view card ]
                        ]
                    ]
    in
        div
            [ class "stack-container" ]
            [ div [ class "stack" ] (List.map viewStackCard (List.indexedMap (,) stack)) ]



-- RESOLVING VIEW.


resView : Res -> Int -> Model -> Html Msg
resView res resTime model =
    div [ class "resolving" ]
        [ viewOtherHand model.otherHand model.otherHover
        , viewHand model.hand True
        , viewStack model.stack
        , viewResTurn
        , viewStatus PlayerA model.life
        , viewStatus PlayerB model.otherLife
        ]


viewResTurn : Html Msg
viewResTurn =
    div
        [ class "turn-indi" ]
        [ text "Resolving..." ]
