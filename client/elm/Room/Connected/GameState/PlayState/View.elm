module PlayState.View exposing (webglView)

import Animation.Types exposing (Anim(..))
import Assets.Types as Assets
import Endgame.View as Endgame
import Model.View as Model
import PlayState.Types exposing (PlayState(..))
import Render.Types as Render
import WebGL


webglView : PlayState -> Render.Params -> Assets.Model -> List WebGL.Entity
webglView playState params assets =
    case playState of
        Playing { game } ->
            Model.view params game assets

        Ended { winner, game, buttons } ->
            let
                resolving =
                    not <| List.isEmpty game.res.resList
            in
            List.concat
                [ Model.view params game assets
                , Endgame.view params assets winner resolving buttons
                ]
