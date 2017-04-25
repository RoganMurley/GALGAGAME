module CharacterSelect exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Card exposing (Card)
import Messages exposing (Msg(..), CharSelectMsg(..), GameMsg(..))
import Util exposing (fromJust)
import Raymarch


-- MODEL


type alias Name =
    String


type alias Character =
    { name : Name
    , imgURL : String
    , cards : ( Card, Card, Card, Card )
    }


type alias Model =
    { characters : List Character
    , selected : List Character
    , hover : Character
    }



-- VIEW


view : Raymarch.Params -> Model -> Html Msg
view (Raymarch.Params frameTime windowDimensions) { characters, selected, hover } =
    let
        characterView : Character -> Html Msg
        characterView ({ name, imgURL } as character) =
            div
                [ class "character-button"
                , onMouseEnter (GameStateMsg (SelectingMsg (SelectingHover name)))
                , onClick (SelectCharacter name)
                , if (List.member character selected) then
                    class "invisible"
                  else
                    class ""
                ]
                [ img [ src ("img/" ++ imgURL), class "character-icon" ] []
                , div [ class "character-name" ] [ text name ]
                ]

        selectedView : List Character -> Html Msg
        selectedView selected =
            let
                chosenView : Character -> Html Msg
                chosenView { name } =
                    div
                        [ class "character-chosen"
                        , onMouseEnter (GameStateMsg (SelectingMsg (SelectingHover name)))
                        ]
                        [ text name ]
            in
                div [ class "ready-up" ]
                    [ div
                        [ class "characters-all-chosen" ]
                        (List.map chosenView selected)
                    , if List.length selected >= 3 then
                        text "Waiting for opponent"
                      else
                        text ""
                    ]

        cardPreviewView : ( Card, Card, Card, Card ) -> Html Msg
        cardPreviewView ( c1, c2, c3, c4 ) =
            div
                [ class "card-preview" ]
                [ Card.view c1
                , Card.view c2
                , Card.view c3
                , Card.view c4
                ]
    in
        div []
            [ div
                [ class "character-select" ]
                [ text "Choose your Gods"
                , div [ class "characters" ]
                    (List.map characterView characters)
                , cardPreviewView ((\{ cards } -> cards) hover)
                , selectedView selected
                ]
            , div [] [ Raymarch.view (Raymarch.Params frameTime windowDimensions) ]
            ]



-- UPDATE


update : CharSelectMsg -> Model -> Model
update msg model =
    case msg of
        SelectingHover n ->
            { model | hover = fromJust (List.head (List.filter (\{ name } -> name == n) model.characters)) }
