module GameState.View exposing (htmlView, paramsFromFlags, webglView)

import Animation.Types exposing (Anim(..))
import Assets.Types as Assets
import Chat.Types as Chat
import DeckBuilding.View as DeckBuilding
import GameState.Messages exposing (Msg(..))
import GameState.Types exposing (GameState(..))
import Html exposing (Html, text)
import Main.Messages as Main
import Main.Types exposing (Flags)
import Mouse exposing (MouseState(..))
import PlayState.View as PlayState
import Render.Types as Render
import Waiting.View as Waiting
import WebGL


htmlView : GameState -> String -> Flags -> Html Main.Msg
htmlView state roomID flags =
    case state of
        Waiting waiting ->
            Waiting.htmlView waiting flags roomID

        Selecting _ ->
            text ""

        Started _ ->
            text ""


webglView : GameState -> Chat.Model -> Render.Params -> Assets.Model -> List WebGL.Entity
webglView state chat params assets =
    case state of
        Waiting waiting ->
            Waiting.webglView waiting params assets

        Selecting selecting ->
            DeckBuilding.webglView params selecting assets

        Started started ->
            PlayState.webglView started chat params assets


paramsFromFlags : Flags -> Render.Params
paramsFromFlags { dimensions, pixelRatio, time, scaling } =
    let
        ( w, h ) =
            dimensions
    in
    { w = w
    , h = h
    , pixelRatio = pixelRatio
    , time = time
    , scaling = scaling
    }
