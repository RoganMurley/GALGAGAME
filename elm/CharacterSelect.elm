module CharacterSelect exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Card exposing (Card, viewCard)
import Messages exposing (Msg(..), CharSelectMsg(..), GameMsg(..))
import Util exposing (fromJust)


-- MODEL


type alias Name =
    String


type alias Character =
    { name : Name
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


view : Model -> Html Msg
view { characters, selected, hover } =
    let
        characterView : Character -> Html Msg
        characterView { name } =
            div
                [ class "character-button"
                , onMouseEnter (GameStateMsg (SelectingMsg (SelectingHover name)))
                , onClick (GameStateMsg (SelectingMsg (SelectingSelect name)))
                , if (contains selected name) then
                    class "invisible"
                  else
                    class ""
                ]
                [ text name ]

        selectedView : SelectedCharacters -> Html Msg
        selectedView s =
            let
                chosenView : Name -> Html Msg
                chosenView n =
                    div
                        [ class "character-chosen"
                        , onMouseEnter (GameStateMsg (SelectingMsg (SelectingHover n)))
                        , onClick (GameStateMsg (SelectingMsg (SelectingDeselect n)))
                        ]
                        [ text n ]
            in
                div [ class "ready-up" ]
                    [ div
                        [ class "characters-all-chosen" ]
                        (List.map chosenView (nameList s))
                    , case s of
                        ThreeSelected _ _ _ ->
                            button
                                [ class "character-ready"
                                , onClick ReadyUp
                                ]
                                [ text "Ready" ]

                        otherwise ->
                            text ""
                    ]

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
update msg model =
    case msg of
        SelectingHover n ->
            { model | hover = fromJust (List.head (List.filter (\{ name } -> name == n) model.characters)) }

        SelectingSelect n ->
            { model | selected = selectCharacter (model.selected) n }

        SelectingDeselect n ->
            { model | selected = deselectCharacter (model.selected) n }


selectCharacter : SelectedCharacters -> Name -> SelectedCharacters
selectCharacter s n =
    case s of
        NoneSelected ->
            OneSelected n

        OneSelected a ->
            TwoSelected n a

        TwoSelected a b ->
            ThreeSelected n b a

        ThreeSelected a b _ ->
            ThreeSelected n a b


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


deselectCharacter : SelectedCharacters -> Name -> SelectedCharacters
deselectCharacter s n =
    case s of
        NoneSelected ->
            NoneSelected

        OneSelected a ->
            if a == n then
                NoneSelected
            else
                OneSelected a

        TwoSelected a b ->
            if a == n then
                OneSelected b
            else if b == n then
                OneSelected a
            else
                TwoSelected a b

        ThreeSelected a b c ->
            if a == n then
                TwoSelected b c
            else if b == n then
                TwoSelected a c
            else if c == n then
                TwoSelected a b
            else
                ThreeSelected a b c
