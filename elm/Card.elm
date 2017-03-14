module Card exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Messages exposing (Msg)


type alias Card =
    { name : String
    , desc : String
    , imgURL : String
    , sfxURL : String
    }


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
