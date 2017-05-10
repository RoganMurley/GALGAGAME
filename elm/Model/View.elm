module Model.View exposing (..)

import Card exposing (Card)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Messages exposing (Msg(..))
import Model.Types exposing (..)
import Model.State exposing (intensity, maxHandLength)
import Vfx


view : Vfx.Params -> Int -> FullModel -> Html Msg
view params resTime model =
    let
        intens : Intensity
        intens =
            intensity model
    in
        div []
            [ viewOtherHand model.otherHand model.otherHover
            , viewHand model.hand
            , viewStack model.stack
            , viewTurn (List.length model.hand == maxHandLength) model.turn
            , viewLife PlayerA model.life
            , viewLife PlayerB model.otherLife

            --, Vfx.view params intens.lower intens.upper resTime
            ]


viewHand : Hand -> Html Msg
viewHand hand =
    let
        cardView : ( Int, Card ) -> Html Msg
        cardView ( index, { name, desc, imgURL } ) =
            div
                [ class "card my-card"
                , onClick (PlayCard index)
                , onMouseEnter (HoverCard (Just index))
                , onMouseLeave (HoverCard Nothing)
                ]
                [ div [ class "card-title" ] [ text name ]
                , div
                    [ class "card-picture"
                    , style [ ( "background-image", "url(\"img/" ++ imgURL ++ "\")" ) ]
                    ]
                    []
                , div [ class "card-desc" ] [ text desc ]
                ]
    in
        div [ class "hand my-hand" ] (List.map cardView (List.indexedMap (,) hand))


viewOtherHand : Int -> HoverCardIndex -> Html Msg
viewOtherHand cardCount hoverIndex =
    let
        cardView : Int -> Html Msg
        cardView index =
            div [ containerClass index hoverIndex ]
                [ div
                    [ class "card other-card"
                    , style [ ( "transform", "rotateZ(" ++ toString (calcRot index) ++ "deg) translateY(" ++ toString (calcTrans index) ++ "px)" ) ]
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
            List.map cardView (List.range 0 (cardCount - 1))

        calcRot : Int -> Int
        calcRot index =
            -2 * (index - (cardCount // 2))

        calcTrans : Int -> Int
        calcTrans index =
            -12 * (abs (index - (cardCount // 2)))
    in
        div [ class "hand other-hand" ] cards


viewTurn : Bool -> WhichPlayer -> Html Msg
viewTurn handFull turn =
    case turn of
        PlayerA ->
            case handFull of
                False ->
                    button [ class "turn-indi pass-button", onClick EndTurn ] [ text "Pass" ]

                True ->
                    button [ class "turn-indi pass-button pass-disabled" ] [ text "Hand full" ]

        PlayerB ->
            div [ class "turn-indi enemy-turn" ] [ text "Opponent's Turn" ]


viewLife : WhichPlayer -> Life -> Html Msg
viewLife which life =
    let
        barWidth : Life -> String
        barWidth barLife =
            (toString (((toFloat barLife) / 50) * 100)) ++ "%"

        whoseLife : String
        whoseLife =
            case which of
                PlayerA ->
                    "life-mine"

                PlayerB ->
                    ""
    in
        div
            [ class "life", class whoseLife ]
            [ div
                [ class "life-bar" ]
                [ div [ class "life-text" ] [ text ("♥ " ++ (toString life) ++ " ♥") ]
                , div [ class "life-health", style [ ( "width", barWidth life ) ] ] []
                ]
            ]


viewStack : Stack -> Html Msg
viewStack stack =
    let
        viewStackCard : StackCard -> Html Msg
        viewStackCard { owner, card } =
            case owner of
                PlayerA ->
                    div [ class "playera stack-card" ] [ Card.view card ]

                PlayerB ->
                    div [ class "playerb stack-card" ] [ Card.view card ]
    in
        div
            [ class "stack-container" ]
            [ div [ class "stack" ] (List.map viewStackCard stack)
            ]



-- RESOLVING VIEW.


resView : Vfx.Params -> Res -> Int -> FullModel -> Html Msg
resView params res resTime model =
    let
        intens : Intensity
        intens =
            intensity model
    in
        div []
            [ viewOtherHand model.otherHand model.otherHover
            , viewResHand model.hand
            , viewStack model.stack
            , viewResTurn
            , viewLife PlayerA model.life
            , viewLife PlayerB model.otherLife

            --, Vfx.view params intens.lower intens.upper resTime
            ]


viewResHand : Hand -> Html Msg
viewResHand hand =
    div [ class "hand my-hand" ] (List.map Card.view hand)


viewResTurn : Html Msg
viewResTurn =
    div [ class "turn-indi" ] [ text "Resolving..." ]
