module Main exposing (..)

import Html exposing (programWithFlags)
import Main.State exposing (initModel, update, subscriptions)
import Main.Types exposing (Flags, Model)
import Main.View exposing (view)
import Main.Messages exposing (Msg)


main : Program Flags Model Msg
main =
    programWithFlags
        { init = \flags -> ( initModel flags, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
