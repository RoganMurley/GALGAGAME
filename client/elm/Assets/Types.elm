module Assets.Types exposing (Handler, Loader, Model, Path)

import Font.Types as Font
import Task exposing (Task)
import Texture.Types as Texture


type alias Model =
    { textures : Texture.Model
    , fonts : Font.Model
    }


type alias Path =
    { name : String
    , path : String
    }


type alias Loader loadable error =
    Path -> Task error loadable


type alias Handler loadable error msg =
    Result error ( String, loadable ) -> msg
