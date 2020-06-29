module Assets.Fetch exposing (fetch)

import Assets.Types exposing (Handler, Loader, Path)
import Task


fetch : Loader loadable error -> Handler loadable error msg -> List Path -> List (Cmd msg)
fetch loader handler paths =
    let
        loaderWithName : Loader ( String, loadable ) error
        loaderWithName path =
            Task.map (\loaded -> ( path.name, loaded )) <| loader path
    in
    List.map (loaderWithName >> Task.attempt handler) paths
