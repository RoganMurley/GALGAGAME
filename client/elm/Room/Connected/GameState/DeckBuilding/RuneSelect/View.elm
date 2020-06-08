module RuneSelect.View exposing (view, webglView)

import Card.View as Card
import Colour
import DeckBuilding.Messages as DeckBuilding
import Game.Types exposing (Context)
import Html as Html exposing (Html, button, div, h2, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Math.Vector2 exposing (vec2)
import Model.View exposing (focusImageView, focusTextView)
import Render.Primitives
import Render.Uniforms exposing (uniColourMag)
import RuneSelect.Messages exposing (Msg(..))
import RuneSelect.Types exposing (Model, RuneCursor(..))
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Context -> Model -> Html DeckBuilding.Msg
view ctx model =
    let
        rune =
            model.carousel.selected
    in
    div []
        [ case model.hover of
            Just { card } ->
                div [ class "text-focus" ] [ focusTextView ctx (Just { owner = PlayerA, card = card }) ]

            -- [ h2 [ class "rune-name" ] [ text card.name ]
            -- ]
            Nothing ->
                div [ class "runes" ]
                    [ h2 [ class "rune-name" ] [ text rune.name ]
                    , button [ class "rune-confirm", class "menu-button", onClick <| DeckBuilding.ConfirmRune model.cursor rune ] [ text "CHOOSE" ]
                    ]
        , Html.map DeckBuilding.RuneSelectMsg nextButton
        , Html.map DeckBuilding.RuneSelectMsg prevButton
        ]


webglView : Model -> Context -> List WebGL.Entity
webglView model ({ w, h, radius } as ctx) =
    let
        focus =
            Maybe.map (\hover -> { owner = PlayerA, card = hover.card }) model.hover
    in
    List.concat
        [ [ Render.Primitives.circle <|
                uniColourMag ctx
                    Colour.tea
                    1
                    { scale = radius * 0.73
                    , position = vec2 (w * 0.5) (h * 0.5)
                    , rotation = 0
                    }
          ]
        , List.concat <| List.map (Card.view ctx) model.entities
        , focusImageView
            focus
            ctx
        ]


prevButton : Html Msg
prevButton =
    div [ class "rune-prev-button", onClick PreviousRune ] []


nextButton : Html Msg
nextButton =
    div [ class "rune-next-button", onClick NextRune ] []
