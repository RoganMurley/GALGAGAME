module PlayState.View exposing (view)

import Animation.Types exposing (Anim(..))
import Assets.Types as Assets
import Endgame.WebGL as Endgame
import GameType exposing (GameType)
import Main.Types exposing (Flags)
import Model.View as Model
import PlayState.Types exposing (PlayState(..))
import WebGL


view : PlayState -> Flags -> Maybe GameType -> Bool -> Assets.Model -> List WebGL.Entity
view playState { time, dimensions, pixelRatio } _ _ assets =
    let
        ( w, h ) =
            dimensions

        params =
            { time = time, w = w, h = h, pixelRatio = pixelRatio }
    in
    case playState of
        Playing { game } ->
            Model.view params game assets

        Ended { winner, game } ->
            let
                resolving =
                    not <| List.isEmpty game.res.resList
            in
            List.concat
                [ Model.view params game assets
                , Endgame.view params assets winner resolving
                ]
