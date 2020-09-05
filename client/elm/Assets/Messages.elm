module Assets.Messages exposing (Msg(..))

import Audio.Messages as Audio
import Font.Messages as Font
import Manifest.Messages as Manifest
import Texture.Messages as Texture


type Msg
    = TextureMsg Texture.Msg
    | FontMsg Font.Msg
    | AudioMsg Audio.Msg
    | ManifestMsg Manifest.Msg
