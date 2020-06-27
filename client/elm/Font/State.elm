module Font.State exposing (fetch, init, update)

import Dict
import Font.Decoders as Font
import Font.Messages exposing (Msg(..))
import Font.Types exposing (Font, Model)
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
        loader : ( String, String ) -> Task Http.Error ( String, Font )
        loader ( name, fontPath ) =
            let
                task : Task.Task Http.Error Font
                task =
                    Http.toTask <|
                        Http.get
                            fontPath
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


fontPaths : List ( String, String )
fontPaths =
    [ ( "myfont", "/fonts/fontmap.json" )
    ]
