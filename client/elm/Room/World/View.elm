module World.View exposing (htmlView, webglView)

import Assets.Types as Assets
import Background.View as Background
import Buttons.View as Buttons
import Card.State as Card
import Card.Types as Card exposing (Card)
import Card.View as Card
import Font.View as Font
import Frame.View as Frame
import Game.State exposing (bareContextInit)
import Game.Types exposing (Context)
import Html exposing (Html, div)
import Line.View as Line
import Main.Types exposing (Flags)
import Math.Vector3 exposing (vec3)
import Quaternion
import Vfx.State as Vfx
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))
import World.Messages exposing (Msg)
import World.Types exposing (Decision, Model, World)
import World.WorldPos exposing (lineToWorldPos)


htmlView : Model -> Html Msg
htmlView _ =
    div [] []


webglView : Model -> Flags -> Assets.Model -> List WebGL.Entity
webglView model { mouse, dimensions } assets =
    let
        { world, choiceButtons, encounterButtons, otherButtons, visitedButtons, time } =
            model

        ctx =
            bareContextInit dimensions assets mouse

        baseVfx =
            Vfx.init

        vfx =
            { baseVfx | rotation = time }

        bodyView =
            case ( world.decision, world.waitPvp ) of
                ( _, Just pvp ) ->
                    waitPvpView pvp ctx

                ( Just decision, _ ) ->
                    decisionView decision time ctx

                _ ->
                    linesView world ctx
    in
    List.concat
        [ Background.radialView vfx ctx
        , bodyView
        , Buttons.view encounterButtons ctx
        , Buttons.view otherButtons ctx
        , Buttons.view visitedButtons ctx
        , Buttons.view choiceButtons ctx
        ]


linesView : World -> Context -> List WebGL.Entity
linesView world ctx =
    let
        lockedLineOptions =
            { col = vec3 0.2 0.2 0.4
            , thickness = 0.01 * ctx.radius
            , alpha = 1
            }

        lineOptions =
            { col = vec3 0.96 0.95 0.37
            , thickness = 0.015 * ctx.radius
            , alpha = 1
            }

        visitedLineOptions =
            { col = vec3 0.96 0.95 0.37
            , thickness = 0.015 * ctx.radius
            , alpha = 1
            }
    in
    List.concat
        [ List.concat <| List.map (\edge -> Line.view (lineToWorldPos ctx edge) lockedLineOptions ctx) world.lockedEdges
        , List.concat <| List.map (\edge -> Line.view (lineToWorldPos ctx edge) lineOptions ctx) world.edges
        , List.concat <| List.map (\edge -> Line.view (lineToWorldPos ctx edge) visitedLineOptions ctx) world.visitedEdges
        ]


decisionView : Decision -> Float -> Context -> List WebGL.Entity
decisionView { cards, title, text } time ctx =
    let
        { w, h } =
            ctx

        size =
            1.4 * max w h

        bodyView : List WebGL.Entity
        bodyView =
            if List.length cards > 0 then
                List.concat <|
                    List.map (Card.view ctx) <|
                        List.indexedMap cardEntity cards

            else
                Font.view
                    "Futura"
                    text
                    { x = w * 0.5
                    , y = h * 0.4
                    , scaleX = 0.00005 * size + 0.001 * sin (time * 0.002)
                    , scaleY = 0.00005 * size + 0.001 * sin (time * 0.002)
                    , color = vec3 (244 / 255) (241 / 255) (94 / 255)
                    }
                    ctx

        cardEntity : Int -> Card -> Card.Entity {}
        cardEntity i card =
            { card = card
            , owner = PlayerA
            , scale = Card.scale
            , rotation = Quaternion.identity
            , position =
                vec3 (-0.3 + toFloat i * 0.2)
                    0
                    0
            }
    in
    List.concat
        [ Font.view
            "Futura"
            title
            { x = w * 0.5
            , y = h * 0.2
            , scaleX = 0.0001 * size + 0.003 * sin (time * 0.005)
            , scaleY = 0.0001 * size + 0.003 * sin (time * 0.005)
            , color = vec3 (244 / 255) (241 / 255) (94 / 255)
            }
            ctx
        , bodyView
        ]


waitPvpView : ( Float, Int ) -> Context -> List WebGL.Entity
waitPvpView ( time, frame ) ctx =
    let
        { w, h } =
            ctx

        size =
            (1.4 + abs (sin (time * 0.001))) * 1.4 * max w h
    in
    List.concat
        [ Frame.view
            "eggs.png"
            (frame // 5)
            256
            { x = w * 0.5
            , y = h * 0.5
            , scaleX = 0.0001 * size + 0.003 * sin (time * 0.005)
            , scaleY = 0.0001 * size + 0.003 * sin (time * 0.005)
            , color = vec3 (244 / 255) (241 / 255) (94 / 255)
            }
            ctx
        ]



-- waitPvpView : Float -> Context -> List WebGL.Entity
-- waitPvpView time ctx =
--     let
--         { w, h } =
--             ctx
--
--         size =
--             (0.4 + abs (sin (time * 0.001))) * 1.4 * max w h
--     in
--     List.concat
--         [ Font.view
--             "Futura"
--             "You feel a strange\nsensation..."
--             { x = w * 0.5
--             , y = h * 0.4
--             , scaleX = 0.0001 * size + 0.003 * sin (time * 0.005)
--             , scaleY = 0.0001 * size + 0.003 * sin (time * 0.005)
--             , color = vec3 (244 / 255) (241 / 255) (94 / 255)
--             }
--             ctx
--         ]
