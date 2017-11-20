module Connected.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Connected.Types exposing (..)
import GameState.View as GameState
import Main.Messages exposing (Msg(..))
import Main.Types exposing (Flags)
import Settings.View as Settings


view : Model -> Flags -> Html Msg
view { game, settings, roomID, players } flags =
    div []
        [ Settings.view settings
        , playersView players
        , GameState.view game roomID flags
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
