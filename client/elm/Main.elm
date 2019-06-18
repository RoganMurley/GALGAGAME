module Main exposing (initFull, main)

import Browser
import Browser.Navigation
import Html exposing (Html)
import Main.Messages exposing (Msg(..))
import Main.State exposing (init, subscriptions, update)
import Main.Types exposing (Flags, InputFlags, Model)
import Main.View exposing (view)
import Url exposing (Url)
import Util exposing (message)


main : Program InputFlags Model Msg
main =
    Browser.application
        { init = initFull
        , view = documentView view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }


documentView : (Model -> Html Msg) -> (Model -> Browser.Document Msg)
documentView viewFunc =
    \model -> { title = "Ring of Worlds", body = [ viewFunc model ] }


initFull : InputFlags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
initFull inputFlags url key =
    let
        ( model, cmd ) =
            init flags url

        flags : Flags
        flags =
            { hostname = inputFlags.hostname
            , httpPort = inputFlags.httpPort
            , seed = inputFlags.seed
            , time = inputFlags.time
            , dimensions = inputFlags.dimensions
            , username = inputFlags.username
            , key = key
            }
    in
    ( model
    , Cmd.batch [ cmd, message GetAuth ]
    )
