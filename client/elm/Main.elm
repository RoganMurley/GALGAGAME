module Main exposing (initFull, main)

import Browser
import Browser.Events exposing (Visibility(..))
import Browser.Navigation
import Html exposing (Html)
import Main.Messages exposing (Msg(..))
import Main.State exposing (init, subscriptions, update)
import Main.Types exposing (Flags, InputFlags, Model)
import Main.View exposing (titleView, view)
import Mouse exposing (MouseState(..))
import Url exposing (Url)


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
    \model -> { title = titleView model, body = [ viewFunc model ] }


initFull : InputFlags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
initFull inputFlags url key =
    let
        flags : Flags
        flags =
            { hostname = inputFlags.hostname
            , httpPort = inputFlags.httpPort
            , seed = inputFlags.seed
            , time = inputFlags.time
            , dimensions = inputFlags.dimensions
            , username = inputFlags.username
            , pixelRatio = inputFlags.pixelRatio
            , scaling = inputFlags.initialScaling
            , backgroundEnabled = inputFlags.initialBackgroundEnabled
            , visits = inputFlags.visits
            , key = key
            , mouse = NoMouse
            , visibility = Visible
            }
    in
    init flags url inputFlags.initialVolume inputFlags.initialMusicVolume
