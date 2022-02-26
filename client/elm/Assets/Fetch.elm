module Assets.Fetch exposing (fetch, fetchErrorToString, resolver)

import Assets.Types exposing (FetchError(..), Handler, Loader, Path)
import Font.Decoders as Font
import Http
import Json.Decode as Json
import Task
import Util exposing (httpErrorToString)


fetch : Loader loadable error -> Handler loadable error msg -> List Path -> List (Cmd msg)
fetch loader handler paths =
    let
        loaderWithName : Loader ( String, loadable ) error
        loaderWithName path =
            Task.map (\loaded -> ( path.name, loaded )) <| loader path
    in
    List.map (loaderWithName >> Task.attempt handler) paths


resolver : Json.Decoder a -> Http.Response String -> Result FetchError a
resolver decoder response =
    case response of
        Http.BadUrl_ s ->
            Err <| FetchHTTPError <| Http.BadUrl s

        Http.Timeout_ ->
            Err <| FetchHTTPError <| Http.Timeout

        Http.NetworkError_ ->
            Err <| FetchHTTPError <| Http.NetworkError

        Http.BadStatus_ metadata body ->
            Err <| FetchHTTPError <| Http.BadStatus metadata.statusCode

        Http.GoodStatus_ metadata body ->
            case Json.decodeString decoder body of
                Err err ->
                    Err <| FetchDecodeError err

                Ok ok ->
                    Ok ok


fetchErrorToString : FetchError -> String
fetchErrorToString fetchError =
    case fetchError of
        FetchHTTPError err ->
            httpErrorToString err

        FetchDecodeError err ->
            Json.errorToString err
