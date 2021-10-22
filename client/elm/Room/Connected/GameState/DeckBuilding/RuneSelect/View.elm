module RuneSelect.View exposing (view)

import Card.View as Card
import Font.View as Font
import Game.Types exposing (Context)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Model.View exposing (focusImageView, focusTextView)
import RuneSelect.Messages exposing (Msg(..))
import RuneSelect.Types exposing (Model, RuneCursor(..))
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Model -> Context -> List WebGL.Entity
view model ({ w, h, tick } as ctx) =
    let
        size =
            1.4 * max w h

        focus =
            Maybe.map
                (\hover -> { owner = PlayerA, card = hover.card })
                model.hover
    in
    List.concat
        [ List.concat <| List.map (Card.view ctx) model.entities
        , focusImageView
            (vec3 0 0.3 0)
            focus
            ctx
        , focusTextView (vec2 0 (-h * 0.1)) focus ctx
        , Font.view
            "Futura"
            model.carousel.selected.name
            { x = w * 0.5
            , y = h * 0.1
            , scaleX = 0.0001 * size + 0.003 * sin (tick * 0.005)
            , scaleY = 0.0001 * size + 0.003 * sin (tick * 0.007)
            , color = vec3 (244 / 255) (241 / 255) (94 / 255)
            }
            ctx
        ]
