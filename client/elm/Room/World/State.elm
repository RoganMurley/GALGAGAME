module World.State exposing (init, mouseDown, mouseUp, receive, tick, update)

import Assets.State as Assets
import Assets.Types as Assets
import Buttons.State as Buttons
import Buttons.Types as Buttons exposing (Button, Buttons)
import Game.State exposing (bareContextInit)
import Json.Decode as Json
import Main.Messages as Main
import Main.Types exposing (Flags)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Mode exposing (Mode(..))
import Mouse
import Ports exposing (log)
import Room.Messages as Room
import Util exposing (message, splitOnColon)
import World.Decoders as World
import World.Messages exposing (Msg(..))
import World.Types exposing (Encounter, Model)
import World.WorldPos exposing (toWorldPos)


init : Model
init =
    { encounterButtons = Buttons.empty
    , otherButtons = Buttons.empty
    , visitedButtons = Buttons.empty
    , choiceButtons = Buttons.empty
    , time = 0
    , world =
        { encounters = []
        , others = []
        , edges = []
        , visited = []
        , visitedEdges = []
        , lockedEdges = []
        , decision = Nothing
        }
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

        { radius, w, h } =
            ctx
    in
    case model.world.decision of
        Nothing ->
            let
                -- Encounter buttons
                encounterButtons : Buttons
                encounterButtons =
                    Buttons.fromList <|
                        List.map encounterToButton model.world.encounters

                encounterToButton : Encounter -> ( String, Button )
                encounterToButton encounter =
                    let
                        { x, y } =
                            toWorldPos ctx encounter
                    in
                    Buttons.entity
                        encounter.guid
                        { x = x
                        , y = y
                        , width = 0.1 * radius
                        , height = 0.1 * radius
                        , btn =
                            Buttons.TextButton
                                { font = "Futura"
                                , text = "?"
                                , textColor = vec3 (0 / 255) (0 / 255) (80 / 255)
                                , bgColor = vec3 (244 / 255) (241 / 255) (94 / 255)
                                , options = [ Buttons.Circular ]
                                }
                        , disabled = False
                        }
                        dt
                        flags.mouse
                        model.encounterButtons

                -- Other buttons
                otherButtons : Buttons
                otherButtons =
                    Buttons.fromList <|
                        List.map otherToButton model.world.others

                otherToButton : ( Float, Float ) -> ( String, Button )
                otherToButton ( x, y ) =
                    let
                        worldPos =
                            toWorldPos ctx { x = x, y = y }
                    in
                    Buttons.entity
                        (String.fromFloat x ++ "/" ++ String.fromFloat y)
                        { x = worldPos.x
                        , y = worldPos.y
                        , width = 0.1 * radius
                        , height = 0.1 * radius
                        , btn =
                            Buttons.TextButton
                                { font = "Futura"
                                , text = "?"
                                , textColor = vec3 (40 / 255) (40 / 255) (40 / 255)
                                , bgColor = vec3 (70 / 255) (70 / 255) (70 / 255)
                                , options = [ Buttons.Circular, Buttons.NoHover ]
                                }
                        , disabled = True
                        }
                        dt
                        flags.mouse
                        model.otherButtons

                -- Visited buttons
                visitedButtons : Buttons
                visitedButtons =
                    Buttons.fromList <|
                        List.map visitedToButton model.world.visited

                visitedToButton : ( Float, Float ) -> ( String, Button )
                visitedToButton ( x, y ) =
                    let
                        worldPos =
                            toWorldPos ctx { x = x, y = y }
                    in
                    Buttons.entity
                        (String.fromFloat x ++ "/" ++ String.fromFloat y)
                        { x = worldPos.x
                        , y = worldPos.y
                        , width = 0.06 * radius
                        , height = 0.06 * radius
                        , btn =
                            Buttons.TextButton
                                { font = "Futura"
                                , text = ""
                                , textColor = vec3 (0 / 255) (0 / 255) (0 / 255)
                                , bgColor = vec3 (244 / 255) (241 / 255) (94 / 255)
                                , options = [ Buttons.Circular, Buttons.NoHover ]
                                }
                        , disabled = True
                        }
                        dt
                        flags.mouse
                        model.visitedButtons
            in
            { model
                | time = model.time + dt
                , encounterButtons = encounterButtons
                , otherButtons = otherButtons
                , visitedButtons = visitedButtons
                , choiceButtons = Buttons.empty
            }

        Just decision ->
            let
                choiceButton : Int -> String -> ( String, Buttons.Button )
                choiceButton index choice =
                    Buttons.entity
                        (String.fromInt index)
                        { x = w * 0.5
                        , y = h * (0.7 + toFloat index * 0.1)
                        , width = 0.4 * radius
                        , height = 0.08 * radius
                        , btn =
                            Buttons.TextButton
                                { font = "Futura"
                                , text = choice ++ "?"
                                , textColor = vec3 (0 / 255) (0 / 255) (0 / 255)
                                , bgColor = vec3 (244 / 255) (241 / 255) (94 / 255)
                                , options = [ Buttons.HoverText <| choice ++ "!" ]
                                }
                        , disabled = False
                        }
                        dt
                        flags.mouse
                        model.choiceButtons

                choiceButtons : Buttons
                choiceButtons =
                    Buttons.fromList <|
                        List.indexedMap choiceButton <|
                            List.map .text decision.choices
            in
            { model
                | time = model.time + dt
                , encounterButtons = Buttons.empty
                , otherButtons = Buttons.empty
                , visitedButtons = Buttons.empty
                , choiceButtons = choiceButtons
            }


mouseUp : Flags -> Assets.Model -> Model -> Mouse.Position -> ( Model, Cmd Main.Msg )
mouseUp _ _ model _ =
    ( model, Cmd.none )


mouseDown : Flags -> Assets.Model -> Model -> Mouse.Position -> ( Model, Cmd Main.Msg )
mouseDown _ _ model { x, y } =
    let
        pos =
            vec2 (toFloat x) (toFloat y)
    in
    case Buttons.hit model.encounterButtons pos of
        Just ( key, _ ) ->
            ( model
            , message <|
                Main.Send <|
                    "joinEncounter:"
                        ++ key
            )

        Nothing ->
            case Buttons.hit model.choiceButtons pos of
                Just ( key, _ ) ->
                    ( model
                    , message <|
                        Main.Send <|
                            "encounterDecision:"
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
