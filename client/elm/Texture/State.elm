module Texture.State exposing (..)

import Dict
import Task
import Texture.Messages exposing (..)
import Texture.Types exposing (..)
import WebGL.Texture
import WebGL.Texture exposing (Texture)


init : Model
init =
    { textures = Dict.empty }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        TexturesLoaded textures ->
            ( saveList m textures, Cmd.none )

        TexturesError error ->
            let
                err =
                    Debug.log "Error loading textures: " error
            in
                ( m, Cmd.none )


save : Model -> ( String, Texture ) -> Model
save model ( name, tex ) =
    { model | textures = Dict.insert name tex model.textures }


saveList : Model -> List ( String, Texture ) -> Model
saveList model textures =
    { model
        | textures =
            Dict.union (Dict.fromList textures) model.textures
    }


load : Model -> String -> Maybe Texture
load { textures } name =
    Dict.get name textures


defaultOptions : WebGL.Texture.Options
defaultOptions =
    WebGL.Texture.defaultOptions


fetchTextures : Cmd Msg
fetchTextures =
    let
        loader : ( String, String ) -> Task.Task WebGL.Texture.Error ( String, Texture )
        loader ( name, texturePath ) =
            let
                task : Task.Task WebGL.Texture.Error Texture
                task =
                    WebGL.Texture.loadWith
                        { defaultOptions
                            | magnify = WebGL.Texture.nearest
                            , minify = WebGL.Texture.nearest
                        }
                        texturePath
            in
                Task.map
                    (\texture -> ( name, texture ))
                    task
    in
        texturePaths
            |> List.map loader
            |> Task.sequence
            |> Task.attempt
                (\result ->
                    case result of
                        Err error ->
                            TexturesError error

                        Ok textures ->
                            TexturesLoaded textures
                )


texturePaths : List ( String, String )
texturePaths =
    [ ( "null", "/img/textures/null.png" )
    , ( "feint", "/img/textures/feint.png" )
    ]


null : Model -> WebGL.Texture.Texture
null m =
    case load m "null" of
        Just t ->
            t

        Nothing ->
            Debug.crash "Failed to load null texture"
