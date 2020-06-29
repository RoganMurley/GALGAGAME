module Assets.Messages exposing (Msg(..))

import Font.Messages as Font
import Texture.Messages as Texture


type Msg
    = TextureMsg Texture.Msg
    | FontMsg Font.Msg
