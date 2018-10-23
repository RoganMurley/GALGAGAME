module PlayState.View exposing (view)

import Animation.State as Animation
import Animation.Types exposing (Anim(GameEnd, NullAnim))
import Endgame.View as Endgame
import Html exposing (Html, div)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Model.View as Model
import PlayState.Types exposing (PlayState(..))
import Resolvable.State exposing (activeAnim)
import Texture.Types as Texture


view : PlayState -> Flags -> Texture.Model -> Html Main.Msg
view playState { time, dimensions } textures =
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
                , Endgame.view 0 NullAnim Nothing
                ]

        Ended { winner, game, replayId } ->
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
                , Endgame.view progress endAnim replayId
                ]
