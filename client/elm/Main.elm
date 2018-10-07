module Main exposing (..)

import Main.State exposing (init, update, subscriptions)
import Main.Types exposing (Flags, Model)
import Main.View exposing (view)
import Main.Messages exposing (Msg(GetAuth, UrlChange))
import Navigation
import Util exposing (message)


main : Program Flags Model Msg
main =
    Navigation.programWithFlags
        UrlChange
        { init = initFull
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initFull : Flags -> Navigation.Location -> ( Model, Cmd Msg )
initFull flags location =
    let
        ( model, cmd ) =
            init flags location
    in
        model ! [ cmd, message GetAuth ]
