module Leaderboard.View exposing (view)

import Html exposing (Html, div, table, td, text, th, tr)
import Html.Attributes exposing (class)
import Leaderboard.Types exposing (Entry, Model)


view : Model -> Html a
view { entries } =
    let
        entryView : Int -> Entry -> Html a
        entryView i { name, xp, level } =
            tr []
                [ td [] [ text <| String.fromInt (i + 1) ]
                , td [ class "username" ] [ text name ]
                , td [] [ text <| String.fromInt level ]
                , td [] [ text <| String.fromInt (floor xp) ]
                ]
    in
    div [ class "leaderboard-box" ]
        [ table []
            (tr []
                [ th [] [ text "Rank" ]
                , th [] [ text "Username" ]
                , th [] [ text "Level" ]
                , th [] [ text "Experience" ]
                ]
                :: List.indexedMap entryView entries
            )
        ]
