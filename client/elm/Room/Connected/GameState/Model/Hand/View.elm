module Hand.View exposing (..)

import Animation.Types exposing (Anim)
import Card.Types exposing (Card)
import GameState.Messages exposing (PlayingOnly(HoverCard, TurnOnly), TurnOnly(PlayCard))
import Hand.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


viewHand : Hand -> HoverCardIndex -> Bool -> Maybe Anim -> Html PlayingOnly
viewHand hand hover resolving anim =
    let
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

        calcTransY : Int -> Float
        calcTransY i =
            let
                index : Int
                index =
                    if (List.length hand) % 2 == 0 && i < cardCount // 2 then
                        i + 1
                    else
                        i
            in
                if isHover hover i then
                    0
                else
                    abs (0.8 * (toFloat (ceiling ((toFloat index) - ((toFloat cardCount) * 0.5)))))

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
                    [ ( "transform"
                      , "translate("
                            ++ (toString <| calcTransX index cardCount)
                            ++ "rem, "
                            ++ (toString <| calcTransY index)
                            ++ "rem) rotate("
                            ++ (toString <| calcRot hover index cardCount)
                            ++ "deg)"
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
    in
        div
            [ class "hand my-hand" ]
            (List.map cardView <| List.indexedMap (,) hand)


viewOtherHand : Int -> HoverCardIndex -> Html msg
viewOtherHand cardCountInt hover =
    let
        cardCount : Float
        cardCount =
            toFloat cardCountInt

        cardView : Int -> Html msg
        cardView index =
            div [ containerClass index hover ]
                [ div
                    [ class "card other-card"
                    , style
                        [ ( "transform"
                          , "translate("
                                ++ (toString <| calcTransX index cardCountInt)
                                ++ "rem, "
                                ++ (toString <| calcTransY index)
                                ++ "rem) rotate("
                                ++ (toString <| -(calcRot hover index cardCountInt))
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


isHover : HoverCardIndex -> Int -> Bool
isHover hoverIndex index =
    case hoverIndex of
        Nothing ->
            False

        Just x ->
            index == x


calcTransX : Int -> Int -> Float
calcTransX index cardCount =
    let
        i =
            toFloat index

        c =
            toFloat cardCount
    in
        -12.0 * (i - (c * 0.5))


calcRot : HoverCardIndex -> Int -> Int -> Float
calcRot hover index cardCount =
    if isHover hover index then
        0
    else
        let
            i =
                toFloat index

            c =
                toFloat cardCount
        in
            -1.5 * (toFloat (ceiling (i - (c * 0.5))))
