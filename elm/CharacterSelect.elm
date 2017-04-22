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


type SelectedCharacters
    = NoneSelected
    | OneSelected Name
    | TwoSelected Name Name
    | ThreeSelected Name Name Name


type alias Model =
    { characters : List Character
    , selected : SelectedCharacters
    , hover : Character
    }



-- VIEW


view : Raymarch.Params -> Model -> Html Msg
view (Raymarch.Params frameTime windowDimensions) { characters, selected, hover } =
    let
        characterView : Character -> Html Msg
        characterView { name, imgURL } =
            div
                [ class "character-button"
                , onMouseEnter (GameStateMsg (SelectingMsg (SelectingHover name)))
                , onClick (SelectCharacter name)
                , if (contains selected name) then
                    class "invisible"
                  else
                    class ""
                ]
                [ img [ src ("img/" ++ imgURL), class "character-icon" ] [], text name ]

        selectedView : SelectedCharacters -> Html Msg
        selectedView s =
            let
                chosenView : Name -> Html Msg
                chosenView n =
                    div
                        [ class "character-chosen"
                        , onMouseEnter (GameStateMsg (SelectingMsg (SelectingHover n)))
                          -- , onClick (GameStateMsg (SelectingMsg (SelectingDeselect n)))
                        ]
                        [ text n ]
            in
                div [ class "ready-up" ]
                    [ div
                        [ class "characters-all-chosen" ]
                        (List.map chosenView (nameList s))
                    , case s of
                        ThreeSelected _ _ _ ->
                            text "Waiting for opponent"

                        otherwise ->
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


nameList : SelectedCharacters -> List Name
nameList s =
    case s of
        NoneSelected ->
            []

        OneSelected a ->
            [ a ]

        TwoSelected a b ->
            [ a, b ]

        ThreeSelected a b c ->
            [ a, b, c ]


contains : SelectedCharacters -> Name -> Bool
contains s n =
    List.member n (nameList s)
