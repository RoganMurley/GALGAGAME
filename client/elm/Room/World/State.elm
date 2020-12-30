module World.State exposing (init, mouseDown, mouseUp, receive, tick, update)

import Assets.State as Assets
import Assets.Types as Assets
import Buttons.State as Buttons
import Buttons.Types as Buttons exposing (Button, Buttons)
import Game.State exposing (bareContextInit)
import Json.Decode as Json
import Main.Messages as Main
import Main.Types exposing (Flags)
import Math.Vector3 exposing (vec3)
import Mode exposing (Mode(..))
import Mouse
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


mouseUp : Flags -> Assets.Model -> Model -> Mouse.Position -> ( Model, Cmd Main.Msg )
mouseUp _ _ model _ =
    ( model, Cmd.none )


mouseDown : Flags -> Assets.Model -> Model -> Mouse.Position -> ( Model, Cmd Main.Msg )
mouseDown _ _ model _ =
    case Buttons.hit model.buttons of
        Just ( key, _ ) ->
            ( model
            , message <|
                Main.Send <|
                    "joinEncounter:"
                        ++ key
            )

        Nothing ->
            ( model, Cmd.none )


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

        "joinEncounter" ->
            message <| Main.RoomMsg <| Room.StartGame Playing (Just content)

        _ ->
            Cmd.none
