module Font.State exposing (fetch, fontPaths, getKerning, init, update)

import Assets.Fetch as Assets
import Assets.Types as Assets
import Dict
import Font.Decoders as Font
import Font.Messages exposing (Msg(..))
import Font.Types exposing (Font, FontChar, FontPath, Model)
import Http
import Main.Messages as Main
import Manifest.Types exposing (Manifest)
import Ports exposing (log)
import Util exposing (httpErrorToString)


init : Model
init =
    { fonts = Dict.empty }


update : Msg -> Model -> ( Model, Cmd Main.Msg )
update msg model =
    case msg of
        FontLoaded ( name, font ) ->
            ( save model name font, Cmd.none )

        FontError errorStr ->
            ( model, log <| "Error loading fonts: " ++ errorStr )


save : Model -> String -> Font -> Model
save model name font =
    { model | fonts = Dict.insert name font model.fonts }


fetch : Manifest -> List (Cmd Msg)
fetch manifest =
    let
        loader : Assets.Loader Font Http.Error
        loader { path } =
            Http.toTask <|
                Http.get path Font.decoder

        handler : Assets.Handler Font Http.Error Msg
        handler result =
            case result of
                Err err ->
                    FontError <| httpErrorToString err

                Ok textures ->
                    FontLoaded textures

        paths : List Assets.Path
        paths =
            List.map
                (\{ name, jsonPath } -> { name = name, path = jsonPath })
                (fontPaths manifest)
    in
    Assets.fetch loader handler paths


fontPaths : Manifest -> List FontPath
fontPaths manifest =
    let
        revise : FontPath -> FontPath
        revise { name, jsonPath, texturePath } =
            { name = name
            , texturePath =
                Dict.get texturePath manifest
                    |> Maybe.withDefault texturePath
            , jsonPath =
                Dict.get jsonPath manifest
                    |> Maybe.withDefault jsonPath
            }
    in
    List.map revise
        [ { name = "Futura"
          , jsonPath = "fonts/futura/fontmap.json"
          , texturePath = "fonts/futura/fontmap.png"
          }
        , { name = "Icons"
          , jsonPath = "fonts/icons/fontmap.json"
          , texturePath = "fonts/icons/fontmap.png"
          }
        ]


getKerning : Font -> Maybe FontChar -> FontChar -> Float
getKerning { kernDict } mPrevChar char =
    case mPrevChar of
        Just prevChar ->
            Dict.get ( prevChar.char, char.char ) kernDict
                |> Maybe.withDefault 0

        Nothing ->
            0
