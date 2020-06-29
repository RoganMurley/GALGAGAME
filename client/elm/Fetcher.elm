module Fetcher exposing (Handler, Loader, Path, fetch)

import Task exposing (Task)


type alias Path =
    { name : String
    , path : String
    }


type alias Loader loadable error =
    Path -> Task error loadable


type alias Handler loadable error msg =
    Result error ( String, loadable ) -> msg


fetch : Loader loadable error -> Handler loadable error msg -> List Path -> List (Cmd msg)
fetch loader handler paths =
    let
        loaderWithName : Loader ( String, loadable ) error
        loaderWithName path =
            Task.map (\loaded -> ( path.name, loaded )) <| loader path
    in
    List.map (loaderWithName >> Task.attempt handler) paths
