module CharacterSelect exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Messages exposing (Msg(..))


-- MODEL


type alias Character =
    { name : String
    }


type SelectedCharacters
    = NoneSelected
    | OneSelected Character
    | TwoSelected Character Character
    | ThreeSelected Character Character Character


type alias Model =
    { characters : List Character
    , selected : SelectedCharacters
    }



-- VIEW


view : Model -> Html Msg
view { characters, selected } =
    let
        characterView : Character -> Html Msg
        characterView { name } =
            div [ class "character-button" ] [ text name ]

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
    in
        div
            [ class "character-select" ]
            [ text "Choose your Characters"
            , div [ class "characters" ]
                (List.map characterView characters)
            , selectedView selected
            ]
