module Lab.State exposing (..)

import Lab.Messages exposing (Msg(..))
import Lab.Types exposing (Model)
import Card.Types exposing (Anim(..))
import Model.Types exposing (WhichPlayer(..))
import GameState.State exposing (tickZero)


init : Model
init =
    let
        initCustom : String
        initCustom =
            """
                precision mediump float;

                uniform float time;
                uniform vec2 resolution;

                void main ()
                {
                    vec2 uv = gl_FragCoord.xy / resolution.xy;
                    gl_FragColor = vec4(uv, .5 * sin(.5 * time), 1.);
                }
            """
    in
        { player = PlayerA
        , anim = Custom initCustom
        , time = 0.0
        , custom = initCustom
        }


update : Model -> Msg -> Model
update model msg =
    case msg of
        SetPlayer player ->
            { model | player = player }

        SetAnim anim ->
            { model | anim = anim }


tickForward : Model -> Float -> Model
tickForward model dt =
    let
        newTime : Float
        newTime =
            if tickZero model.time then
                0.0
            else
                model.time + dt
    in
        { model | time = newTime }
