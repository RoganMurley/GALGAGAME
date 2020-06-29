module Font.State exposing (fetch, fontPaths, init, update)

import Dict
import Font.Decoders as Font
import Font.Messages exposing (Msg(..))
import Font.Types exposing (Font, FontPath, Model)
import Http
import Ports exposing (log)
import Task exposing (Task)


init : Model
init =
    { fonts = Dict.empty }


update : Msg -> Model -> ( Model, Cmd Msg )
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
        loader : FontPath -> Task Http.Error ( String, Font )
        loader { name, jsonPath } =
            let
                task : Task.Task Http.Error Font
                task =
                    Http.toTask <|
                        Http.get
                            jsonPath
                            Font.decoder
            in
            Task.map
                (\font -> ( name, font ))
                task

        handler : Result Http.Error ( String, Font ) -> Msg
        handler result =
            case result of
                Err _ ->
                    FontError "HTTP error"

                Ok textures ->
                    FontLoaded textures
    in
    List.map (loader >> Task.attempt handler) fontPaths


fontPaths : List FontPath
fontPaths =
    [ { name = "Rock Salt", jsonPath = "/fonts/rock_salt/fontmap.json", texturePath = "/fonts/rock_salt/fontmap.png" }
    , { name = "Futura", jsonPath = "/fonts/futura/fontmap.json", texturePath = "/fonts/futura/fontmap.png" }
    ]
