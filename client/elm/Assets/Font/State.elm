module Font.State exposing (fetch, fontPaths, init, update)

import Assets.Fetch as Assets
import Assets.Types as Assets
import Dict
import Font.Decoders as Font
import Font.Messages exposing (Msg(..))
import Font.Types exposing (Font, FontPath, Model)
import Http
import Main.Messages as Main
import Ports exposing (log)


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


fetch : List (Cmd Msg)
fetch =
    let
        loader : Assets.Loader Font Http.Error
        loader { path } =
            Http.toTask <|
                Http.get path Font.decoder

        handler : Assets.Handler Font Http.Error Msg
        handler result =
            case result of
                Err _ ->
                    FontError "HTTP error"

                Ok textures ->
                    FontLoaded textures

        paths : List Assets.Path
        paths =
            List.map
                (\{ name, jsonPath } -> { name = name, path = jsonPath })
                fontPaths
    in
    Assets.fetch loader handler paths


fontPaths : List FontPath
fontPaths =
    [ { name = "Rock Salt", jsonPath = "/fonts/rock_salt/fontmap.json", texturePath = "/fonts/rock_salt/fontmap.png" }
    , { name = "Futura", jsonPath = "/fonts/futura/fontmap.json", texturePath = "/fonts/futura/fontmap.png" }
    ]
