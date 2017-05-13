module Card.View exposing (view)

import Card.Types exposing (Card)
import Html exposing (..)
import Html.Attributes exposing (..)


view : Card -> Html msg
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
