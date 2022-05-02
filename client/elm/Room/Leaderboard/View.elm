module Leaderboard.View exposing (view)

import Html exposing (Html, div, table, td, text, th, tr)
import Html.Attributes exposing (class)
import Leaderboard.Types exposing (Entry, Model)


view : Model -> Html a
view { entries } =
    let
        entryView : Entry -> Html a
        entryView { name, xp, level } =
            tr []
                [ td [] [ text name ]
                , td [] [ text <| String.fromInt (floor xp) ++ "xp" ]
                , td [] [ text <| "Level " ++ String.fromInt level ]
                ]
    in
    div []
        [ table []
            (tr []
                [ th [] [ text "Username" ]
                , th [] [ text "Xp" ]
                , th [] [ text "Level" ]
                ]
                :: List.map entryView entries
            )
        ]
