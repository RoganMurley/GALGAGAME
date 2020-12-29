module World.State exposing (init, receive, tick, update)

import Assets.State as Assets
import Buttons.State as Buttons
import Buttons.Types as Buttons exposing (Button, Buttons)
import Game.State exposing (bareContextInit)
import Json.Decode as Json
import Main.Messages as Main
import Main.Types exposing (Flags)
import Math.Vector3 exposing (vec3)
import Ports exposing (log)
import Room.Messages as Room
import Util exposing (message, splitOnColon)
import World.Decoders as World
import World.Messages exposing (Msg(..))
import World.Types exposing (Encounter, Model)


init : Model
init =
    { buttons = Buttons.empty
    , time = 0
    , world = []
    }


update : Model -> Msg -> Flags -> ( Model, Cmd Main.Msg )
update model msg _ =
    case msg of
        JoinWorld ->
            ( model, message <| Main.Send <| "world:" )

        LoadWorld world ->
            ( { model | world = world }, Cmd.none )


tick : Flags -> Model -> Float -> Model
tick flags model dt =
    let
        ctx =
            bareContextInit flags.dimensions Assets.init flags.mouse

        { w, h, radius } =
            ctx

        buttons : Buttons
        buttons =
            Buttons.fromList <|
                List.map toButton model.world

        toButton : Encounter -> ( String, Button )
        toButton encounter =
            Buttons.entity
                encounter.guid
                { x = encounter.x * w
                , y = encounter.y * h
                , width = 0.1 * radius
                , height = 0.1 * radius
                , btn =
                    Buttons.TextButton
                        { font = "Futura"
                        , text = encounter.name
                        , textColor = vec3 (0 / 255) (0 / 255) (80 / 255)
                        , bgColor = vec3 (244 / 255) (241 / 255) (94 / 255)
                        , options = [ Buttons.Circular ]
                        }
                , disabled = False
                }
                dt
                flags.mouse
                model.buttons
    in
    { model
        | time = model.time + dt
        , buttons = buttons
    }


receive : String -> Cmd Main.Msg
receive msg =
    let
        ( command, content ) =
            splitOnColon msg
    in
    case command of
        "world" ->
            case Json.decodeString World.decoder content of
                Ok world ->
                    message <|
                        Main.RoomMsg <|
                            Room.WorldMsg <|
                                LoadWorld world

                Err err ->
                    log <| Json.errorToString err

        _ ->
            Cmd.none
