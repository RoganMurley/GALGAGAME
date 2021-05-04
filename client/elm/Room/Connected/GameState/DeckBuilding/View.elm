module DeckBuilding.View exposing (webglView)

import Assets.Types as Assets
import Background.View exposing (radialView)
import Buttons.View as Buttons
import Carousel exposing (Carousel)
import DeckBuilding.Messages exposing (Msg(..))
import DeckBuilding.Types exposing (Character, Model)
import Font.Types as Font
import Font.View as Font
import Game.State exposing (bareContextInit)
import Game.Types exposing (Context)
import Math.Vector3 exposing (vec3)
import Mouse exposing (MouseState(..))
import Render.Types as Render
import RuneSelect.Types as RuneSelect exposing (RuneCursor(..))
import RuneSelect.View as RuneSelect
import WebGL
import WebGL.Texture as WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


webglView : Render.Params -> Model -> Assets.Model -> List WebGL.Entity
webglView { w, h } model assets =
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
                    [ radialView model.vfx
                    , if model.ready then
                        waitingView model.vfx.rotation

                      else
                        charactersView model.characters model.vfx.rotation
                    , Buttons.view model.buttons
                    ]


charactersView : Carousel Character -> Float -> Context -> List WebGL.Entity
charactersView characters tick ctx =
    let
        character =
            characters.selected

        { w, h, radius } =
            ctx

        size =
            6 * radius
    in
    Font.view
        "Futura"
        character.name
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
        "WAITING FOR\nOPPONENT"
        { x = w * 0.5 - 0.003 * size
        , y = h * 0.4
        , scaleX = 0.0001 * size + 0.003 * sin (tick * 0.005)
        , scaleY = 0.0001 * size + 0.003 * sin (tick * 0.007)
        , color = vec3 (244 / 255) (241 / 255) (94 / 255)
        }
        ctx
