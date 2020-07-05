module PlayState.View exposing (view)

import Animation.Types exposing (Anim(..))
import Assets.Types as Assets
import Endgame.WebGL as Endgame
import Main.Types exposing (Flags)
import Model.View as Model
import PlayState.Types exposing (PlayState(..))
import WebGL


view : PlayState -> Flags -> Assets.Model -> List WebGL.Entity
view playState { time, dimensions, pixelRatio } assets =
    let
        ( w, h ) =
            dimensions

        params =
            { time = time, w = w, h = h, pixelRatio = pixelRatio }
    in
    case playState of
        Playing { game } ->
            Model.view params game assets

        Ended { winner, game, buttonEntities } ->
            let
                resolving =
                    not <| List.isEmpty game.res.resList
            in
            List.concat
                [ Model.view params game assets
                , Endgame.view params assets winner resolving buttonEntities
                ]
