module Manifest.State exposing (fetch, init, update)

import Assets.Fetch as Assets
import Assets.Messages as Assets
import Assets.Types as Assets
import Font.State as Font
import Http
import Main.Messages as Main
import Manifest.Decoders as Manifest
import Manifest.Messages exposing (Msg(..))
import Manifest.Types exposing (Manifest)
import Ports exposing (log)
import Texture.State as Texture


init : Maybe Manifest
init =
    Nothing


update : Msg -> Maybe Manifest -> ( Maybe Manifest, Cmd Main.Msg )
update msg m =
    case msg of
        ManifestLoaded ( _, manifest ) ->
            let
                fetchFont : List (Cmd Main.Msg)
                fetchFont =
                    List.map
                        (Cmd.map (Main.AssetsMsg << Assets.FontMsg))
                        (Font.fetch manifest)

                fetchTextures : List (Cmd Main.Msg)
                fetchTextures =
                    List.map
                        (Cmd.map (Main.AssetsMsg << Assets.TextureMsg))
                        (Texture.fetch manifest)
            in
            ( Just manifest, Cmd.batch <| fetchFont ++ fetchTextures )

        ManifestError errorStr ->
            ( m, log <| "Error loading asset manifest: " ++ errorStr )


fetch : List (Cmd Msg)
fetch =
    let
        loader : Assets.Loader Manifest Http.Error
        loader { path } =
            Http.toTask <|
                Http.get path Manifest.decoder

        handler : Assets.Handler Manifest Http.Error Msg
        handler result =
            case result of
                Err _ ->
                    ManifestError "HTTP error"

                Ok textures ->
                    ManifestLoaded textures

        paths : List Assets.Path
        paths =
            [ { name = "rev-manifest.json", path = "/rev-manifest.json" } ]
    in
    Assets.fetch loader handler paths
