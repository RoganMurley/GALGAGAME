module Lab.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lab.Messages exposing (Msg(..))
import Lab.Types exposing (Model)
import Raymarch.Types exposing (Params(..))
import Animation.View as Animation
import Card.Types exposing (Anim(..))
import Model.Types exposing (WhichPlayer(..))


view : Params -> Model -> Html Msg
view params { player, anim, time } =
    div [ class "lab" ]
        [ div [ class "sets" ]
            [ fieldset []
                [ legend [] [ text "Animation:" ]
                , radio (SetAnim Slash) "anim" "Slash"
                , radio (SetAnim Obliterate) "anim" "Obliterate"
                ]
            , fieldset []
                [ legend [] [ text "Owner:" ]
                , radio (SetPlayer PlayerA) "which-player" "Player A"
                , radio (SetPlayer PlayerB) "which-player" "Player B"
                ]
            ]
        , Animation.view params time (Just ( player, anim ))
        ]


radio : msg -> String -> String -> Html msg
radio msg fGroup value =
    label []
        [ input
            [ type_ "radio"
            , onClick msg
            , name fGroup
            ]
            []
        , text value
        ]
