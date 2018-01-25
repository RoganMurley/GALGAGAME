module Hand.View exposing (..)

import Animation.Types exposing (Anim(Draw))
import Card.Types exposing (Card)
import Ease
import GameState.Messages exposing (PlayingOnly(HoverCard, TurnOnly), TurnOnly(PlayCard))
import Hand.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model.Types exposing (WhichPlayer(..))
import Resolvable.State exposing (resTickMax)


viewHand : Hand -> HoverCardIndex -> Float -> Bool -> Maybe Anim -> Html PlayingOnly
viewHand finalHand hover resTick resolving anim =
    let
        ( hand, drawingCard ) =
            case anim of
                Just (Draw PlayerA) ->
                    ( List.take (List.length finalHand - 1) finalHand
                    , List.head <| List.reverse finalHand
                    )

                otherwise ->
                    ( finalHand, Nothing )

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

        cardCount : Int
        cardCount =
            List.length hand

        conditionalClasses : Int -> String
        conditionalClasses index =
            if isHover hover index then
                " card-hover"
            else
                ""

        cardView : ( Int, Card ) -> Html PlayingOnly
        cardView ( index, { name, desc, imgURL } ) =
            div
                [ class <| "my-card-container" ++ (conditionalClasses index)
                , style
                    [ transformCss <|
                        transformEase Ease.outQuint
                            (resTick / resTickMax)
                            (buildTransform PlayerA
                                { cardCount = cardCount
                                , hover = hover
                                , index = index
                                }
                            )
                            (buildTransform PlayerA
                                { cardCount = List.length finalHand
                                , hover = hover
                                , index = index
                                }
                            )
                    ]
                ]
                [ div
                    ([ class <| "card my-card" ++ (conditionalClasses index) ]
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

        drawingCardView : Html msg
        drawingCardView =
            case drawingCard of
                Nothing ->
                    text ""

                Just { name, desc, imgURL } ->
                    div
                        [ style
                            [ transformCss <|
                                transformEase Ease.outQuint
                                    (resTick / resTickMax)
                                    { x = 100
                                    , y = -10.0
                                    , r = 30.0
                                    }
                                    (buildTransform PlayerA
                                        { cardCount = List.length finalHand
                                        , hover = hover
                                        , index = List.length finalHand - 1
                                        }
                                    )
                            ]
                        ]
                        [ div
                            [ class "card my-card" ]
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
            ((List.map cardView <| List.indexedMap (,) hand)
                ++ [ drawingCardView ]
            )


viewOtherHand : Int -> HoverCardIndex -> Float -> Maybe Anim -> Html msg
viewOtherHand finalCardCount hover resTick anim =
    let
        ( cardCount, drawingCard ) =
            case anim of
                Just (Draw PlayerB) ->
                    ( finalCardCount - 1
                    , Just finalCardCount
                    )

                otherwise ->
                    ( finalCardCount, Nothing )

        cardView : Int -> Html msg
        cardView index =
            div [ containerClass index hover ]
                [ div
                    [ class "card other-card"
                    , style
                        [ transformCss <|
                            transformEase Ease.outQuint
                                (resTick / resTickMax)
                                (buildTransform PlayerB
                                    { cardCount = cardCount
                                    , hover = hover
                                    , index = index
                                    }
                                )
                                (buildTransform PlayerB
                                    { cardCount = finalCardCount
                                    , hover = hover
                                    , index = index
                                    }
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

        drawingCardView : Html msg
        drawingCardView =
            case drawingCard of
                Nothing ->
                    text ""

                Just index ->
                    div [ containerClass index hover ]
                        [ div
                            [ class "card other-card"
                            , style
                                [ transformCss <|
                                    transformEase Ease.outQuint
                                        (resTick / resTickMax)
                                        { x = 100
                                        , y = 10.0
                                        , r = -30.0
                                        }
                                        (buildTransform PlayerB
                                            { cardCount = finalCardCount
                                            , hover = hover
                                            , index = index - 1
                                            }
                                        )
                                ]
                            ]
                            []
                        ]
    in
        div [ class "hand other-hand" ]
            ((List.map cardView <| List.range 0 <| cardCount - 1)
                ++ [ drawingCardView ]
            )


isHover : HoverCardIndex -> Int -> Bool
isHover hoverIndex index =
    case hoverIndex of
        Nothing ->
            False

        Just x ->
            index == x


calcTransX : HandIndex -> Float
calcTransX { index, cardCount } =
    let
        i =
            toFloat index

        c =
            toFloat cardCount
    in
        12.0 * (i - (c * 0.5) + 1.0)


calcTransY : HandIndex -> Float
calcTransY { hover, index, cardCount } =
    let
        i : Float
        i =
            if cardCount % 2 == 0 && index < cardCount // 2 then
                toFloat <| index + 1
            else
                toFloat index

        c : Float
        c =
            toFloat cardCount
    in
        if isHover hover index then
            0
        else
            abs (0.8 * (toFloat (ceiling (i - (c * 0.5)))))


calcRot : HandIndex -> Float
calcRot { hover, index, cardCount } =
    if isHover hover index then
        0
    else
        let
            i =
                toFloat index

            c =
                toFloat cardCount
        in
            1.5 * (toFloat (ceiling (i - (c * 0.5))))


type alias Transform =
    { x : Float
    , y : Float
    , r : Float
    }


buildTransform : WhichPlayer -> HandIndex -> Transform
buildTransform which handIndex =
    let
        x =
            calcTransX handIndex

        y =
            case which of
                PlayerA ->
                    calcTransY handIndex

                PlayerB ->
                    -(calcTransY handIndex)

        r =
            case which of
                PlayerA ->
                    calcRot handIndex

                PlayerB ->
                    -(calcRot handIndex)
    in
        { x = x, y = y, r = r }


transformCss : Transform -> ( String, String )
transformCss { x, y, r } =
    ( "transform"
    , "translate("
        ++ toString x
        ++ "rem, "
        ++ toString y
        ++ "rem) rotate("
        ++ toString r
        ++ "deg)"
    )


transformEase : Ease.Easing -> Float -> Transform -> Transform -> Transform
transformEase easing t start final =
    let
        diff : Transform
        diff =
            { x = final.x - start.x
            , y = final.y - start.y
            , r = final.r - start.r
            }
    in
        { x = start.x + (easing t) * diff.x
        , y = start.y + (easing t) * diff.y
        , r = start.r + (easing t) * diff.r
        }
