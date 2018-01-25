module Lab.State exposing (..)

import Animation.Types exposing (Anim(..))
import Lab.Messages exposing (Msg(..))
import Lab.Types exposing (Model)
import Model.Types exposing (WhichPlayer(..))
import Resolvable.State exposing (tickZero)


init : Model
init =
    let
        initCustom : String
        initCustom =
            """
                precision mediump float;

                uniform float time;
                uniform vec2 resolution;
                uniform float flipper;

                void main ()
                {
                    vec2 uv = gl_FragCoord.xy / resolution.xy;
                    uv = abs(vec2(flipper, flipper) - uv);
                    gl_FragColor = vec4(uv, .5 * sin(.5 * time), 1.);
                }
            """
    in
        { player = PlayerA
        , anim = Slash PlayerA 10
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


tick : Model -> Float -> Model
tick model dt =
    let
        newTime : Float
        newTime =
            if tickZero model.time then
                0.0
            else
                model.time + dt
    in
        { model | time = newTime }
