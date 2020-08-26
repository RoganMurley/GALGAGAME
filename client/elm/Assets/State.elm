module Assets.State exposing (init, update)

import Assets.Messages exposing (Msg(..))
import Assets.Types exposing (Model)
import Font.State as Font
import Main.Messages as Main
import Manifest.State as Manifest
import Texture.State as Texture


init : Model
init =
    { textures = Texture.init
    , fonts = Font.init
    , manifest = Manifest.init
    }


update : Msg -> Model -> ( Model, Cmd Main.Msg )
update msg model =
    case msg of
        TextureMsg textureMsg ->
            let
                ( newTextures, cmd ) =
                    Texture.update textureMsg model.textures
            in
            ( { model | textures = newTextures }
            , cmd
            )

        FontMsg fontMsg ->
            let
                ( newFonts, cmd ) =
                    Font.update fontMsg model.fonts
            in
            ( { model | fonts = newFonts }
            , cmd
            )

        ManifestMsg manifestMsg ->
            let
                ( newManifest, cmd ) =
                    Manifest.update manifestMsg model.manifest
            in
            ( { model | manifest = newManifest }
            , cmd
            )
