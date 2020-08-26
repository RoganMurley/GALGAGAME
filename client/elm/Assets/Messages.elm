module Assets.Messages exposing (Msg(..))

import Font.Messages as Font
import Manifest.Messages as Manifest
import Texture.Messages as Texture


type Msg
    = TextureMsg Texture.Msg
    | FontMsg Font.Msg
    | ManifestMsg Manifest.Msg
