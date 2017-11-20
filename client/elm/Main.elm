module Main exposing (..)

import Main.State exposing (init, update, subscriptions)
import Main.Types exposing (Flags, Model)
import Main.View exposing (view)
import Main.Messages exposing (Msg(UrlChange))
import Navigation


main : Program Flags Model Msg
main =
    Navigation.programWithFlags
        UrlChange
        { init = \flags location -> ( init flags location, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
