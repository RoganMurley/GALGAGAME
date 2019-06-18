module PlayState.View exposing (view)

import Animation.State as Animation
import Animation.Types exposing (Anim(..))
import Endgame.View as Endgame
import GameType exposing (GameType)
import Html exposing (Html, div)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Model.View as Model
import PlayState.Types exposing (PlayState(..))
import Resolvable.State exposing (activeAnim)
import Texture.Types as Texture


view : PlayState -> Flags -> Maybe GameType -> Texture.Model -> Html Main.Msg
view playState { time, dimensions, username } gameType textures =
    let
        ( w, h ) =
            dimensions

        params =
            { time = time, w = w, h = h }
    in
    case playState of
        Playing { game } ->
            div []
                [ Model.view params game textures
                , Endgame.view 0 NullAnim Nothing Nothing gameType username
                ]

        Ended { winner, game, replayId, xp } ->
            let
                anim =
                    activeAnim game.res

                { resList, tick } =
                    game.res

                resolving =
                    not <| List.isEmpty resList

                ( endAnim, progress ) =
                    if resolving then
                        ( anim, Animation.progress anim tick )

                    else
                        ( GameEnd winner, 1.0 )
            in
            div []
                [ Model.view params game textures
                , Endgame.view progress endAnim replayId xp gameType username
                ]
