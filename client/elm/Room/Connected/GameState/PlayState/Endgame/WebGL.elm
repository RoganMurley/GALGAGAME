module Endgame.WebGL exposing (animView, buttonEntities, view)

import Animation.Types exposing (Anim(..))
import Assets.State as Assets
import Assets.Types as Assets
import Collision exposing (hitTest)
import Connected.Messages as Connected
import Ease
import Font.View as Font
import Game.State exposing (bareContextInit)
import Game.Types exposing (ButtonEntity, Context)
import GameState.Messages as GameState
import GameType exposing (GameType(..))
import Main.Messages as Main
import Math.Matrix4 exposing (makeLookAt, makeOrtho, makeRotate, makeScale3)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (vec3)
import PlayState.Messages as PlayState
import Render.Primitives
import Render.Shaders
import Render.Types as Render
import Room.Messages as Room
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


animView : Context -> List WebGL.Entity
animView ({ w, h, radius, anim, progress } as ctx) =
    case anim of
        GameEnd winner ->
            let
                ( text, backgroundColor ) =
                    case winner of
                        Just PlayerA ->
                            ( "Victory", vec3 (30 / 255) (200 / 255) (30 / 255) )

                        Just PlayerB ->
                            ( "Defeat", vec3 (200 / 255) (30 / 255) (30 / 255) )

                        Nothing ->
                            ( "Draw", vec3 (255 / 255) (255 / 255) (255 / 255) )

                color =
                    vec3 (244 / 255) (241 / 255) (94 / 255)

                size =
                    radius * 3
            in
            List.concat
                [ [ Render.Primitives.quad Render.Shaders.matte
                        { rotation = makeRotate pi (vec3 0 0 1)
                        , scale = makeScale3 w h 1
                        , color = backgroundColor
                        , alpha = 0.8 * Ease.outCubic progress
                        , pos = vec3 (w * 0.5) (h * 0.5) 0
                        , worldRot = makeRotate 0 (vec3 0 0 1)
                        , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                        , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                        }
                  ]
                , Font.view
                    "Rock Salt"
                    text
                    { x = w * 0.5 - 0.003 * size
                    , y = h * 0.4
                    , scaleX = 0.3 * Ease.outBounce (progress * progress)
                    , scaleY = 0.3 * Ease.outBounce (progress * progress)
                    , color = vec3 (40 / 255) (20 / 255) (20 / 255)
                    }
                    ctx
                , Font.view
                    "Rock Salt"
                    text
                    { x = w * 0.5
                    , y = h * 0.4
                    , scaleX = 0.3 * Ease.outBounce progress
                    , scaleY = 0.3 * Ease.outBounce progress
                    , color = color
                    }
                    ctx
                ]

        _ ->
            []


view : Render.Params -> Assets.Model -> Maybe WhichPlayer -> Bool -> List ButtonEntity -> List WebGL.Entity
view { w, h } assets winner resolving buttons =
    let
        ctx =
            bareContextInit ( w, h ) assets Nothing
    in
    if resolving then
        []

    else
        List.concat
            [ animView { ctx | anim = GameEnd winner, progress = 1 }
            , buttonsView ctx buttons
            ]


buttonsView : Context -> List ButtonEntity -> List WebGL.Entity
buttonsView ctx buttons =
    let
        buttonView : ButtonEntity -> List WebGL.Entity
        buttonView { font, text, position, scale, disabled, hover } =
            let
                color =
                    case ( disabled, hover ) of
                        ( True, _ ) ->
                            vec3 0 0 0

                        ( False, True ) ->
                            vec3 1 1 1

                        ( False, False ) ->
                            vec3 0.9 0.9 0.9
            in
            Font.view
                font
                text
                { x = Math.Vector2.getX position
                , y = Math.Vector2.getY position
                , scaleX = scale * 0.002
                , scaleY = scale * 0.002
                , color = color
                }
                ctx
    in
    List.concat <| List.map buttonView buttons


buttonEntities : Render.Params -> Maybe Vec2 -> Maybe GameType -> Maybe WhichPlayer -> Maybe String -> List ButtonEntity
buttonEntities { w, h } mouse mGameType winner mReplayId =
    case mGameType of
        Nothing ->
            []

        Just gameType ->
            let
                ctx =
                    bareContextInit ( w, h ) Assets.init mouse

                playMsg =
                    Main.RoomMsg
                        << Room.ConnectedMsg
                        << Connected.GameStateMsg
                        << GameState.PlayStateMsg

                buttonScale =
                    128

                -- Play Again
                playAgainPos =
                    vec2 (ctx.w * 0.3) (ctx.h * 0.6)

                playAgainIsHover =
                    case mouse of
                        Just mousePos ->
                            hitTest playAgainPos buttonScale { position = mousePos }

                        Nothing ->
                            False

                playAgainOnClick =
                    case ( gameType, winner ) of
                        ( TutorialGame, Just PlayerA ) ->
                            playMsg PlayState.GotoComputerGame

                        _ ->
                            playMsg <| PlayState.PlayingOnly PlayState.Rematch

                -- Replay
                replayPos =
                    vec2 (ctx.w * 0.7) (ctx.h * 0.6)

                replayIsHover =
                    case mouse of
                        Just mousePos ->
                            hitTest replayPos buttonScale { position = mousePos }

                        Nothing ->
                            False

                ( replayIsDisabled, replayOnClick ) =
                    case mReplayId of
                        Just replayId ->
                            ( True, Just <| playMsg <| PlayState.GotoReplay replayId )

                        Nothing ->
                            ( False, Nothing )
            in
            [ { position = playAgainPos
              , scale = 32
              , rotation = 0
              , text = "Play Again"
              , font = "Futura"
              , disabled = False
              , hover = playAgainIsHover
              , onClick = Just playAgainOnClick
              }
            , { position = replayPos
              , scale = 32
              , rotation = 0
              , text = "Watch Replay"
              , font = "Futura"
              , disabled = replayIsDisabled
              , hover = replayIsHover
              , onClick = replayOnClick
              }
            ]
