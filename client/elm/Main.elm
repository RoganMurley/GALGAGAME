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
        { init = \flags location -> ( init flags location, message GetAuth )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
