module Icon.View exposing (view)

import Font.Types exposing (Entity)
import Font.View as Font
import Game.Types exposing (Context)
import WebGL


view : String -> Entity -> Context -> List WebGL.Entity
view iconName entity ctx =
    let
        mEncoded =
            case iconName of
                "?" ->
                    Just "a"

                "zzz" ->
                    Just "b"

                "egg" ->
                    Just "c"

                _ ->
                    Nothing
    in
    case mEncoded of
        Just encoded ->
            Font.view "Icons" encoded entity ctx

        Nothing ->
            []
