module Animation.State exposing (..)

import Card.Types exposing (Anim(..))
import Model.Types exposing (WhichPlayer(..))
import Animation.Shaders as Shaders
import Raymarch.Types exposing (Uniforms)
import WebGL exposing (Shader)


animToFragmentShader : Maybe ( WhichPlayer, Anim ) -> Shader {} Uniforms {}
animToFragmentShader params =
    case params of
        Just ( PlayerA, Slash ) ->
            Shaders.slashA

        Just ( PlayerB, Slash ) ->
            Shaders.slashB

        Just ( _, Obliterate ) ->
            Shaders.obliterate

        Nothing ->
            Shaders.null
