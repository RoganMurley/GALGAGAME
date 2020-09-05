module Assets.Types exposing (Handler, Loader, Model, Path)

import Audio.Types as Audio
import Font.Types as Font
import Manifest.Types exposing (Manifest)
import Task exposing (Task)
import Texture.Types as Texture


type alias Model =
    { textures : Texture.Model
    , fonts : Font.Model
    , audio : Audio.Model
    , manifest : Maybe Manifest
    }


type alias Path =
    { name : String
    , path : String
    }


type alias Loader loadable error =
    Path -> Task error loadable


type alias Handler loadable error msg =
    Result error ( String, loadable ) -> msg
