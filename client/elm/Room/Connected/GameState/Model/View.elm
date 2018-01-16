module Model.View exposing (..)

import Card.Types exposing (Card)
import Card.View as Card
import Connected.Messages as Connected
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Main.Messages as Main
import Resolvable.State exposing (activeAnim, activeModel, nextActiveModel, resolving)
import Resolvable.Types as Resolvable
import Room.Messages as Room
import GameState.Messages exposing (..)
import Model.Types exposing (..)
import Model.State exposing (maxHandLength)
import Model.ViewModel exposing (..)


cardWidth : Float
cardWidth =
    14.0


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
            viewHand model.hand viewModel.hover False
        , viewStack model.stack
        , Html.map playingOnly <|
            viewTurn (List.length model.hand == maxHandLength) model.turn
        , viewStatus PlayerA model.life
        , viewStatus PlayerB model.otherLife
        ]


viewHand : Hand -> HoverCardIndex -> Bool -> Html PlayingOnly
viewHand hand hoverIndex resolving =
    let
        isHover : Int -> Bool
        isHover index =
            case hoverIndex of
                Nothing ->
                    False

                Just x ->
                    index == x

        mouseActions : Int -> List (Attribute PlayingOnly)
        mouseActions index =
            let
                clickActions =
                    if resolving then
                        []
                    else
                        [ onClick <|
                            TurnOnly <|
                                PlayCard index
                        ]
            in
                [ onMouseEnter <|
                    HoverCard <|
                        Just index
                , onMouseLeave <|
                    HoverCard Nothing
                ]
                    ++ clickActions

        calcRot : Int -> Float
        calcRot i =
            if isHover i then
                0
            else
                -1.5 * (toFloat (ceiling ((toFloat i) - (cardCount * 0.5))))

        calcTransX : Int -> Float
        calcTransX index =
            -12.0 * ((toFloat index) - (cardCount * 0.5))

        calcTransY : Int -> Float
        calcTransY i =
            let
                index : Int
                index =
                    if (List.length hand) % 2 == 0 && toFloat i < cardCount * 0.5 then
                        i + 1
                    else
                        i
            in
                if isHover i then
                    0
                else
                    abs (0.8 * (toFloat (ceiling ((toFloat index) - (cardCount * 0.5)))))

        cardCount : Float
        cardCount =
            toFloat <| List.length hand

        conditionalClasses : Int -> String
        conditionalClasses index =
            if isHover index then
                " card-hover"
            else
                ""

        cardView : ( Int, Card ) -> Html PlayingOnly
        cardView ( index, { name, desc, imgURL } ) =
            div
                [ class <| "my-card-container" ++ (conditionalClasses index)
                , style
                    [ ( "transform"
                      , "translate("
                            ++ (toString <| calcTransX index)
                            ++ "rem, "
                            ++ (toString <| calcTransY index)
                            ++ "rem) rotate("
                            ++ (toString <| calcRot index)
                            ++ "deg)"
                      )
                    ]
                ]
                [ div
                    ([ class <| "card my-card" ++ (conditionalClasses index)
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


viewOtherHand : Int -> HoverCardIndex -> Html msg
viewOtherHand cardCountInt hoverIndex =
    let
        cardCount : Float
        cardCount =
            toFloat cardCountInt

        cardView : Int -> Html msg
        cardView index =
            div [ containerClass index hoverIndex ]
                [ div
                    [ class "card other-card"
                    , style
                        [ ( "transform"
                          , "translate("
                                ++ (toString <| calcTransX index)
                                ++ "rem, "
                                ++ (toString <| calcTransY index)
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

        cards : List (Html msg)
        cards =
            List.map cardView (List.range 0 (cardCountInt - 1))

        calcRot : Int -> Float
        calcRot i =
            let
                index : Int
                index =
                    if cardCountInt % 2 == 0 && toFloat i < (toFloat cardCountInt) * 0.5 then
                        i + 1
                    else
                        i
            in
                1.5 * (toFloat (ceiling ((toFloat index) - (cardCount * 0.5))))

        calcTransX : Int -> Float
        calcTransX index =
            -12.0 * ((toFloat index) - (cardCount * 0.5))

        calcTransY : Int -> Float
        calcTransY i =
            let
                index : Int
                index =
                    if cardCountInt % 2 == 0 && toFloat i < (toFloat cardCountInt) * 0.5 then
                        i + 1
                    else
                        i
            in
                -0.8 * abs (1.5 * (toFloat (ceiling ((toFloat index) - (cardCount * 0.5)))))
    in
        div [ class "hand other-hand" ] cards


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


resView : Model.ViewModel.ViewModel -> Resolvable.ResolveData -> Float -> Html Main.Msg
resView vm { model, stackCard } time =
    let
        stack : List StackCard
        stack =
            stackCard :: model.stack
    in
        div [ class "game-container resolving", style [ screenshakeStyle vm.shake time ] ]
            [ viewOtherHand model.otherHand model.otherHover
            , Html.map playingOnly <|
                viewHand model.hand vm.hover True
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
