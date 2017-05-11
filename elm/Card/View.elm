module Card.View exposing (view)

import Card.Types exposing (Card)
import Html exposing (..)
import Html.Attributes exposing (..)
import Main.Messages exposing (Msg)


view : Card -> Html Msg
view { name, desc, imgURL } =
    div
        [ class "card"
        ]
        [ div [ class "card-title" ] [ text name ]
        , div
            [ class "card-picture"
            , style [ ( "background-image", "url(\"img/" ++ imgURL ++ "\")" ) ]
            ]
            []
        , div [ class "card-desc" ] [ text desc ]
        ]
