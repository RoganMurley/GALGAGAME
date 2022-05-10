module PlayState.View exposing (htmlView, webglView)

import Animation.Types exposing (Anim(..))
import Assets.Types as Assets
import Chat.Types as Chat
import Endgame.View as Endgame
import Html exposing (Html, text)
import Main.Types exposing (Flags)
import Model.View as Model
import PlayState.Types exposing (PlayState(..))
import Render.Types as Render
import Resolvable.State exposing (resolving)
import WebGL


webglView : PlayState -> Chat.Model -> Render.Params -> Assets.Model -> Bool -> List WebGL.Entity
webglView playState chat params assets isReplay =
    case playState of
        Playing { game } ->
            Model.view params game chat assets

        Ended { winner, game, buttons, aftermath } ->
            List.concat
                [ Model.view params game chat assets
                , Endgame.view params assets winner (resolving game.res) aftermath buttons isReplay
                ]


htmlView : PlayState -> Flags -> Html a
htmlView playstate flags =
    case playstate of
        Playing _ ->
            text ""

        Ended { game, aftermath } ->
            Endgame.htmlView aftermath flags (resolving game.res)
