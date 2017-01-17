module CharacterSelect exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Card exposing (Card, viewCard)
import Messages exposing (Msg(..), CharSelectMsg(..), GameMsg(..))
import Util exposing (fromJust)


-- MODEL


type alias Character =
    { name : String
    , cards : ( Card, Card, Card, Card )
    }


type SelectedCharacters
    = NoneSelected
    | OneSelected Character
    | TwoSelected Character Character
    | ThreeSelected Character Character Character


type alias Model =
    { characters : List Character
    , selected : SelectedCharacters
    , hover : Character
    }



-- VIEW


view : Model -> Html Msg
view { characters, selected, hover } =
    let
        characterView : Character -> Html Msg
        characterView { name } =
            div
                [ class "character-button", onMouseEnter (GameStateMsg (SelectingMsg (SelectingHover name))) ]
                [ text name ]

        selectedView : SelectedCharacters -> Html Msg
        selectedView s =
            case s of
                NoneSelected ->
                    div [ class "character-count" ] [ text "0/3" ]

                OneSelected _ ->
                    div [ class "character-count" ] [ text "1/3" ]

                TwoSelected _ _ ->
                    div [ class "character-count" ] [ text "2/3" ]

                ThreeSelected _ _ _ ->
                    button [ class "character-lock" ] [ text "lock" ]

        cardPreviewView : ( Card, Card, Card, Card ) -> Html Msg
        cardPreviewView ( c1, c2, c3, c4 ) =
            div
                [ class "card-preview" ]
                [ viewCard c1
                , viewCard c2
                , viewCard c3
                , viewCard c4
                ]
    in
        div
            [ class "character-select" ]
            [ text "Choose your Characters"
            , div [ class "characters" ]
                (List.map characterView characters)
            , cardPreviewView ((\{ cards } -> cards) hover)
            , selectedView selected
            ]



-- UPDATE


update : CharSelectMsg -> Model -> Model
update msg ({ characters } as model) =
    case msg of
        SelectingHover characterName ->
            { model | hover = fromJust (List.head (List.filter (\{ name } -> name == characterName) characters)) }
