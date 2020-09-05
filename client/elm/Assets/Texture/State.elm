module Texture.State exposing (defaultOptions, fetch, init, load, save, texturePaths, update, with, with2, with3, with4, with5)

import Assets.Fetch as Assets
import Assets.Types as Assets
import Dict
import Font.State exposing (fontPaths)
import Main.Messages as Main
import Manifest.Types exposing (Manifest)
import Ports exposing (log)
import Texture.Messages exposing (Msg(..))
import Texture.Types exposing (Model)
import WebGL.Texture exposing (Error(..), Texture)


init : Model
init =
    { textures = Dict.empty }


update : Msg -> Model -> ( Model, Cmd Main.Msg )
update msg m =
    case msg of
        TexturesLoaded textures ->
            ( save m textures, Cmd.none )

        TexturesError error ->
            let
                errorStr : String
                errorStr =
                    case error of
                        LoadError ->
                            "Load error"

                        SizeError w h ->
                            "Size error " ++ String.fromInt w ++ ", " ++ String.fromInt h
            in
            ( m, log <| "Error loading textures: " ++ errorStr )


save : Model -> ( String, Texture ) -> Model
save model ( name, tex ) =
    { model | textures = Dict.insert name tex model.textures }


load : Model -> String -> Maybe Texture
load { textures } name =
    Dict.get name textures


defaultOptions : WebGL.Texture.Options
defaultOptions =
    WebGL.Texture.defaultOptions


fetch : Manifest -> List (Cmd Msg)
fetch manifest =
    let
        loader : Assets.Loader Texture WebGL.Texture.Error
        loader { path } =
            WebGL.Texture.loadWith
                { defaultOptions
                    | magnify = WebGL.Texture.nearest
                    , minify = WebGL.Texture.nearest
                }
                path

        handler : Assets.Handler Texture WebGL.Texture.Error Msg
        handler result =
            case result of
                Err error ->
                    TexturesError error

                Ok textures ->
                    TexturesLoaded textures

        paths : List Assets.Path
        paths =
            texturePaths manifest
                ++ List.map
                    (\{ name, texturePath } -> { name = name, path = texturePath })
                    (fontPaths manifest)
    in
    Assets.fetch loader handler paths


texturePaths : Manifest -> List Assets.Path
texturePaths manifest =
    Dict.filter
        (\key _ -> String.startsWith "img/textures/" key)
        manifest
        |> Dict.toList
        |> List.map
            (\( revisedName, path ) ->
                { name =
                    String.dropLeft
                        (String.length "img/textures/")
                        revisedName
                , path = path
                }
            )


with : Model -> String -> (Texture -> List a) -> List a
with model path func =
    case load model path of
        Just texture ->
            func texture

        Nothing ->
            []


with2 : Model -> String -> String -> (Texture -> Texture -> List a) -> List a
with2 model pathA pathB func =
    case ( load model pathA, load model pathB ) of
        ( Just textureA, Just textureB ) ->
            func textureA textureB

        _ ->
            []


with3 : Model -> String -> String -> String -> (Texture -> Texture -> Texture -> List a) -> List a
with3 model pathA pathB pathC func =
    case ( load model pathA, load model pathB, load model pathC ) of
        ( Just textureA, Just textureB, Just textureC ) ->
            func textureA textureB textureC

        _ ->
            []


with4 : Model -> String -> String -> String -> String -> (Texture -> Texture -> Texture -> Texture -> List a) -> List a
with4 model pathA pathB pathC pathD func =
    case ( load model pathA, load model pathB ) of
        ( Just textureA, Just textureB ) ->
            case ( load model pathC, load model pathD ) of
                ( Just textureC, Just textureD ) ->
                    func textureA textureB textureC textureD

                _ ->
                    []

        _ ->
            []


with5 : Model -> String -> String -> String -> String -> String -> (Texture -> Texture -> Texture -> Texture -> Texture -> List a) -> List a
with5 model pathA pathB pathC pathD pathE func =
    case ( load model pathA, load model pathB ) of
        ( Just textureA, Just textureB ) ->
            case ( load model pathC, load model pathD ) of
                ( Just textureC, Just textureD ) ->
                    case load model pathE of
                        Just textureE ->
                            func textureA textureB textureC textureD textureE

                        Nothing ->
                            []

                _ ->
                    []

        _ ->
            []
