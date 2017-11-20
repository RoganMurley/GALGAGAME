module Connected.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Connected.Types exposing (..)
import GameState.View as GameState
import Main.Messages exposing (Msg(..))
import Settings.View as Settings
import Raymarch.Types as Raymarch


view : Model -> String -> String -> Raymarch.Params -> Html Msg
view { game, settings, roomID, players } hostname httpPort params =
    div []
        [ Settings.view settings
        , playersView players
        , GameState.view game roomID hostname httpPort params
        ]


playersView : ( Maybe String, Maybe String ) -> Html msg
playersView ( pa, pb ) =
    let
        playerView : Maybe String -> Bool -> Html msg
        playerView mName me =
            let
                meClass : String
                meClass =
                    if me then
                        "me"
                    else
                        ""
            in
                div
                    [ class <| "player-name " ++ meClass ]
                    [ text <| Maybe.withDefault "" mName ]
    in
        div [ class "player-layer" ]
            [ playerView pb True
            , playerView pa False
            ]
