module Hand.View exposing (..)

import Animation.Types exposing (Anim)
import Card.Types exposing (Card)
import GameState.Messages exposing (PlayingOnly(HoverCard, TurnOnly), TurnOnly(PlayCard))
import Hand.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model.Types exposing (WhichPlayer(..))


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
                    [ buildTransform
                        PlayerA
                        { cardCount = cardCount
                        , hover = hover
                        , index = index
                        }
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
viewOtherHand cardCount hover =
    let
        cardView : Int -> Html msg
        cardView index =
            div [ containerClass index hover ]
                [ div
                    [ class "card other-card"
                    , style
                        [ buildTransform
                            PlayerB
                            { cardCount = cardCount
                            , hover = hover
                            , index = index
                            }
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
    in
        div [ class "hand other-hand" ]
            (List.map cardView <| List.range 0 <| cardCount - 1)


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
        -12.0 * (i - (c * 0.5))


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
            -1.5 * (toFloat (ceiling (i - (c * 0.5))))


buildTransform : WhichPlayer -> HandIndex -> ( String, String )
buildTransform which handIndex =
    let
        transX =
            calcTransX handIndex

        transY =
            case which of
                PlayerA ->
                    calcTransY handIndex

                PlayerB ->
                    -(calcTransY handIndex)

        rot =
            case which of
                PlayerA ->
                    calcRot handIndex

                PlayerB ->
                    -(calcRot handIndex)
    in
        ( "transform"
        , "translate("
            ++ toString transX
            ++ "rem, "
            ++ toString transY
            ++ "rem) rotate("
            ++ toString rot
            ++ "deg)"
        )
