module CharacterSelect.View exposing (view)

import CharacterSelect.Messages exposing (Msg(..))
import CharacterSelect.Types exposing (Character, Model)
import Game.State exposing (bareContextInit)
import Game.Types exposing (Context)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, height, src, width)
import Html.Events exposing (onClick, onMouseEnter)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Render.Types as Render
import Render.Uniforms exposing (uni, uniColourMag)
import Texture.Types as Texture
import WebGL


view : Render.Params -> Model -> Texture.Model -> Html Msg
view { w, h } { characters, selected } textures =
    let
        ctx =
            bareContextInit ( w, h ) textures

        characterView : Character -> Html Msg
        characterView character =
            div
                [ class "character-button"
                , onMouseEnter <| Hover character
                , onClick <| Select character
                , if List.member character selected then
                    class "invisible"

                  else
                    class ""
                ]
                [ img [ src ("/img/" ++ character.imgURL), class "character-icon" ] []
                ]

        selectedView : Html Msg
        selectedView =
            let
                chosenView : Character -> Html Msg
                chosenView character =
                    div
                        [ class "character-chosen"
                        , onMouseEnter <| Hover character
                        ]
                        [ img [ src ("/img/" ++ character.imgURL), class "character-icon" ] [] ]

                unchosen : List (Html Msg)
                unchosen =
                    List.repeat
                        (3 - List.length selected)
                        (div [ class "character-unchosen" ] [])
            in
            div []
                [ div
                    [ class "characters-all-chosen" ]
                    (List.map chosenView selected ++ unchosen)
                , div
                    [ class "ready-up" ]
                    [ if List.length selected >= 3 then
                        text "Waiting for opponent"

                      else
                        text ""
                    ]
                ]
    in
    div [ class "character-select" ]
        [ WebGL.toHtml
            [ width w
            , height h
            , class "webgl-canvas"
            ]
          <|
            List.concat <|
                List.map ((|>) ctx)
                    [ backgroundRingView
                    , circlesView
                    ]
        , div []
            [ text "Select your Species"
            , div [ class "characters" ] <| List.map characterView characters
            , selectedView
            ]
        ]


backgroundRingView : Context -> List WebGL.Entity
backgroundRingView ({ w, h, radius } as ctx) =
    let
        centre =
            vec2 (w / 2) (h / 2)
    in
    [ Render.Primitives.fullCircle <|
        uniColourMag ctx
            (vec3 0.12 0.12 0.12)
            1.0
            { scale = 0.8 * radius
            , position = centre
            , rotation = 0
            }
    , Render.Primitives.fullCircle <|
        uniColourMag ctx
            (vec3 0.08 0.08 0.08)
            1.0
            { scale = 0.52 * radius
            , position = centre
            , rotation = 0
            }
    ]


circlesView : Context -> List WebGL.Entity
circlesView ({ w, h, radius } as ctx) =
    let
        centre =
            vec2 (w / 2) (h / 2)
    in
    List.map (Render.Primitives.circle << uni ctx)
        [ { scale = 0.8 * radius, position = centre, rotation = 0 }
        , { scale = 0.52 * radius, position = centre, rotation = 0 }
        ]
