module DeckBuilding.View exposing (webglView)

import Assets.Types as Assets
import Background.View exposing (radialView)
import Buttons.View as Buttons
import Colour
import DeckBuilding.Messages exposing (Msg(..))
import DeckBuilding.Types exposing (Model)
import Font.Types as Font
import Font.View as Font
import Game.State exposing (bareContextInit)
import Game.Types exposing (Context)
import Math.Vector3 exposing (vec3)
import Mouse exposing (MouseState(..))
import Players exposing (Player, Players)
import Render.Types as Render
import RuneSelect.Types as RuneSelect exposing (RuneCursor(..))
import RuneSelect.View as RuneSelect
import WebGL
import WebGL.Texture as WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


webglView : Render.Params -> Model -> Players -> Assets.Model -> List WebGL.Entity
webglView { w, h } model players assets =
    let
        ctx =
            bareContextInit ( w, h ) assets NoMouse
    in
    List.concat <|
        List.map ((|>) ctx) <|
            case model.runeSelect of
                Just runeSelect ->
                    [ radialView model.vfx
                    , RuneSelect.view runeSelect
                    , Buttons.view model.buttons
                    ]

                Nothing ->
                    let
                        subview =
                            if model.ready then
                                waitingView

                            else
                                titleView
                    in
                    [ radialView model.vfx
                    , subview model.vfx.depth
                    , Buttons.view model.buttons
                    , questView players.pa
                    ]


titleView : Float -> Context -> List WebGL.Entity
titleView tick ctx =
    let
        { w, h, radius } =
            ctx

        size =
            6 * radius
    in
    Font.view
        "Futura"
        "DECKBUILDING"
        { x = w * 0.5 - 0.003 * size
        , y = h * 0.2
        , scaleX = 0.00006 * size + 0.003 * sin (tick * 0.005)
        , scaleY = 0.00006 * size + 0.003 * sin (tick * 0.007)
        , color = vec3 (244 / 255) (241 / 255) (94 / 255)
        }
        ctx


waitingView : Float -> Context -> List WebGL.Entity
waitingView tick ctx =
    let
        { w, h } =
            ctx

        size =
            1.4 * max w h
    in
    Font.view
        "Futura"
        "OPPONENT\nCHOOSING..."
        { x = w * 0.5 - 0.003 * size
        , y = h * 0.4
        , scaleX = 0.0001 * size + 0.003 * sin (tick * 0.005)
        , scaleY = 0.0001 * size + 0.003 * sin (tick * 0.007)
        , color = vec3 (244 / 255) (241 / 255) (94 / 255)
        }
        ctx


questView : Maybe Player -> Context -> List WebGL.Entity
questView mPlayer ctx =
    let
        { tick } =
            ctx
    in
    case mPlayer of
        Just { quests } ->
            case List.head quests of
                Just quest ->
                    let
                        { w, h, radius } =
                            ctx

                        size =
                            3 * radius
                    in
                    Font.view
                        "Futura"
                        ("QUEST: " ++ quest)
                        { x = w * 0.5 - 0.003 * size
                        , y = h * 0.9
                        , scaleX = 0.00006 * size + 0.003 * sin (tick * 0.005)
                        , scaleY = 0.00006 * size + 0.003 * sin (tick * 0.007)
                        , color = Colour.white
                        }
                        ctx

                Nothing ->
                    []

        Nothing ->
            []
