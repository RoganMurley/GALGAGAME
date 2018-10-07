module Clock.Hand exposing (..)

import Animation.State as Animation
import Animation.Types exposing (Anim(..))
import Clock.Card exposing (CardEntity, cardEntity, colour, dissolvingCardEntity)
import Clock.Primitives as Primitives
import Clock.Types exposing (ClockParams, GameEntity)
import Clock.Uniforms exposing (uniforms)
import Ease
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe
import Texture.State as Texture
import Texture.Types as Texture
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))
import Util exposing (interpFloat, interp2D, to3d)


cardDimensions : ClockParams -> { width : Float, height : Float, spacing : Float }
cardDimensions { radius } =
    { width = 0.1 * radius
    , height = 0.1 * radius
    , spacing = 35.0
    }


origin : ClockParams -> WhichPlayer -> Int -> Vec3
origin ({ w, h } as params) which count =
    let
        { width, height, spacing } =
            cardDimensions params

        x =
            w / 2 - 0.5 * (width + spacing) * (toFloat <| count - 1)

        y =
            case which of
                PlayerA ->
                    h - height

                PlayerB ->
                    height
    in
        vec3 x y 0


rotation : WhichPlayer -> Int -> Int -> Float
rotation which i count =
    let
        magnitude =
            0.05 * (toFloat i - (toFloat count * 0.5))
    in
        case which of
            PlayerA ->
                pi + magnitude

            PlayerB ->
                -magnitude


position : ClockParams -> WhichPlayer -> Int -> Int -> Vec3
position params which index count =
    let
        { width, spacing } =
            cardDimensions params

        sign =
            case which of
                PlayerA ->
                    1

                PlayerB ->
                    -1

        y =
            let
                i =
                    if count % 2 == 0 && index < count // 2 then
                        toFloat <| index + 1
                    else
                        toFloat index

                c =
                    toFloat count
            in
                sign * (abs <| 4 * (toFloat <| ceiling (i - c * 0.5)))
    in
        Math.Vector3.add
            (origin params which count)
        <|
            Math.Vector3.add
                (vec3 (toFloat index * (width + spacing)) 0 0)
            <|
                vec3 0 y 0


handView : ClockParams -> List (CardEntity { index : Int }) -> Maybe ( Float, Maybe Anim ) -> Texture.Model -> List WebGL.Entity
handView ({ w, h } as params) handEntities resInfo textures =
    let
        resTick =
            Maybe.withDefault 0.0 <|
                Maybe.map Tuple.first resInfo

        anim =
            Maybe.join <|
                Maybe.map Tuple.second resInfo

        progress =
            Animation.progress anim resTick

        mainView : List WebGL.Entity
        mainView =
            List.concat <|
                List.map (cardEntity params textures) handEntities

        extraView : List WebGL.Entity
        extraView =
            overdrawView params progress anim textures
    in
        mainView ++ extraView


otherHandView : ClockParams -> List (GameEntity {}) -> Maybe ( Float, Maybe Anim ) -> Texture.Model -> List WebGL.Entity
otherHandView ({ w, h } as params) otherHandEntities resInfo textures =
    let
        resTick =
            Maybe.withDefault 0.0 <|
                Maybe.map Tuple.first resInfo

        anim =
            Maybe.join <|
                Maybe.map Tuple.second resInfo

        progress =
            Animation.progress anim resTick

        { width, height } =
            cardDimensions params

        mTexture =
            Texture.load textures "noise"

        entity : GameEntity {} -> List WebGL.Entity
        entity { position, rotation, scale } =
            let
                rot =
                    makeRotate rotation <| vec3 0 0 1

                pos =
                    to3d position
            in
                case mTexture of
                    Just texture ->
                        [ Primitives.roundedBox <|
                            uniforms
                                ( floor w, floor h )
                                texture
                                pos
                                rot
                                (makeScale3 (scale * 0.7 * width) (scale * height) 1)
                                (colour PlayerB)
                        ]

                    Nothing ->
                        []

        mainView : List WebGL.Entity
        mainView =
            List.concat <| List.map entity otherHandEntities

        extraView : List WebGL.Entity
        extraView =
            overdrawView params progress anim textures
    in
        mainView ++ extraView


overdrawView : ClockParams -> Float -> Maybe Anim -> Texture.Model -> List WebGL.Entity
overdrawView ({ w, h } as params) resTick anim textures =
    case anim of
        Just (Overdraw owner card) ->
            let
                progress =
                    Animation.progress anim resTick

                sign =
                    case owner of
                        PlayerA ->
                            1

                        PlayerB ->
                            -1

                disintegrateProgress =
                    Ease.inQuint (resTick / Animation.animMaxTick anim)

                entity =
                    { owner = owner
                    , card = card
                    , position =
                        interp2D
                            progress
                            (vec2 w h)
                            (vec2 (w / 2) (h / 2))
                    , rotation = interpFloat progress pi (pi - sign * 0.05 * pi)
                    , scale = interpFloat progress 1 4
                    }
            in
                dissolvingCardEntity
                    params
                    textures
                    disintegrateProgress
                    entity

        _ ->
            []
