module RuneSelect.View exposing (view, webglView)

import Card.View as Card
import Colour
import DeckBuilding.Messages as DeckBuilding
import Game.Types exposing (Context)
import Html as Html exposing (Html, button, div, h2, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Math.Vector2 exposing (vec2)
import Render.Primitives
import Render.Uniforms exposing (uniColourMag)
import RuneSelect.Messages exposing (Msg(..))
import RuneSelect.Types exposing (Model, RuneCursor(..))
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Model -> Html DeckBuilding.Msg
view model =
    let
        rune =
            model.carousel.selected
    in
    div []
        [ div [ class "runes" ]
            [ h2 [ class "rune-name" ] [ text rune.name ]
            , button [ class "rune-confirm", class "menu-button", onClick <| DeckBuilding.ConfirmRune model.cursor rune ] [ text "CHOOSE" ]
            ]
        , Html.map DeckBuilding.RuneSelectMsg nextButton
        , Html.map DeckBuilding.RuneSelectMsg prevButton
        ]


webglView : Model -> Context -> List WebGL.Entity
webglView model ({ w, h, radius } as ctx) =
    let
        rune =
            model.carousel.selected
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
        , Card.view ctx
            { position = vec2 (w * 0.5) (h * 0.5 - radius * 0.7)
            , rotation = pi
            , scale = 1.4
            , card = rune.cards.b
            , owner = PlayerA
            }
        , Card.view ctx
            { position = vec2 (w * 0.5 + radius * 0.7) (h * 0.5)
            , rotation = pi
            , scale = 1.4
            , card = rune.cards.a
            , owner = PlayerA
            }
        , Card.view ctx
            { position = vec2 (w * 0.5) (h * 0.5 + radius * 0.7)
            , rotation = pi
            , scale = 1.4
            , card = rune.cards.d
            , owner = PlayerA
            }
        , Card.view ctx
            { position = vec2 (w * 0.5 - radius * 0.7) (h * 0.5)
            , rotation = pi
            , scale = 1.4
            , card = rune.cards.c
            , owner = PlayerA
            }
        ]


prevButton : Html Msg
prevButton =
    div [ class "rune-prev-button", onClick PreviousRune ] []


nextButton : Html Msg
nextButton =
    div [ class "rune-next-button", onClick NextRune ] []
