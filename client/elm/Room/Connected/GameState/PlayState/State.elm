module PlayState.State exposing (carry, get, map, mouseClick, mouseMove, resolveOutcomeStr, tick, update, updatePlayingOnly, updateTurnOnly)

import Animation.Types exposing (Anim(HandFullPass, Play, Windup))
import Audio exposing (playSound)
import Game.Encoders
import Game.State as Game
import Game.Types as Game
import Json.Decode as Json
import List.Extra as List
import Main.Messages as Main
import Main.Types exposing (Flags)
import Math.Vector2 exposing (vec2)
import Mode exposing (Mode)
import Model.Decoders as Model
import Model.Diff exposing (Diff, initDiff)
import Model.State as Model
import Model.Types exposing (Model)
import Mouse exposing (Position)
import Navigation
import PlayState.Decoders as PlayState
import PlayState.Messages exposing (Msg(..), PlayingOnly(..), TurnOnly(..))
import PlayState.Types as PlayState exposing (PlayState(..))
import Resolvable.Decoders
import Resolvable.State as Resolvable
import Resolvable.Types as Resolvable
import Util exposing (message, send, unsafeForceDecode)
import WhichPlayer.Types exposing (WhichPlayer(..))


update : Msg -> PlayState -> Mode -> Flags -> ( PlayState, Cmd Main.Msg )
update msg state mode flags =
    case msg of
        PlayingOnly playingOnly ->
            updatePlayingOnly playingOnly state mode flags

        HoverOtherOutcome otherHover ->
            case state of
                Playing ({ game } as playing) ->
                    ( Playing { playing | game = { game | otherHover = otherHover } }
                    , Cmd.none
                    )

                _ ->
                    ( state, Cmd.none )

        DamageOutcome dmg ->
            case state of
                Playing ({ game } as playing) ->
                    let
                        hover =
                            Game.hoverDamage game.hover dmg
                    in
                    ( Playing { playing | game = { game | hover = hover } }
                    , Cmd.none
                    )

                _ ->
                    ( state, Cmd.none )

        GotoReplay replayId ->
            ( state, Navigation.load <| "/replay/" ++ replayId )

        ReplaySaved replayId ->
            case state of
                Ended ended ->
                    ( Ended { ended | replayId = Just replayId }, Cmd.none )

                _ ->
                    ( state, Cmd.none )

        StatChange statChange ->
            case state of
                Ended ended ->
                    ( Ended { ended | xp = Just statChange }, Cmd.none )

                _ ->
                    ( state, Cmd.none )


updatePlayingOnly : PlayingOnly -> PlayState -> Mode.Mode -> Flags -> ( PlayState, Cmd Main.Msg )
updatePlayingOnly msg state mode flags =
    case mode of
        Mode.Spectating ->
            ( state, Cmd.none )

        Mode.Playing ->
            case msg of
                Rematch ->
                    case state of
                        Ended _ ->
                            ( state, send flags "rematch:" )

                        _ ->
                            ( state, Cmd.none )

                HoverCard hover ->
                    let
                        sound =
                            case Game.getHoverIndex hover of
                                Just _ ->
                                    playSound "/sfx/hover.wav"

                                Nothing ->
                                    Cmd.none
                    in
                    state
                        ! [ message <|
                                Main.Send <|
                                    "hover:"
                                        ++ Game.Encoders.encodeHoverSelf hover
                          , sound
                          ]

                IllegalPass ->
                    case state of
                        Playing { game } ->
                            let
                                -- Construct the ResolveData clientside to avoid latency.
                                newState : PlayState
                                newState =
                                    resolveOutcome initial resDiffList state (Just state)

                                initial : Model
                                initial =
                                    game.res.final

                                resDiffList : List Resolvable.ResolveDiffData
                                resDiffList =
                                    [ { diff = initDiff
                                      , anim = HandFullPass
                                      , animDamage = ( 0, 0 )
                                      , stackCard = Nothing
                                      }
                                    ]
                            in
                            newState ! []

                        _ ->
                            state ! []

                TurnOnly turnOnly ->
                    updateTurnOnly turnOnly state flags


updateTurnOnly : TurnOnly -> PlayState -> Flags -> ( PlayState, Cmd Main.Msg )
updateTurnOnly msg state flags =
    case state of
        Playing { game } ->
            if game.res.final.turn == PlayerA then
                case msg of
                    EndTurn ->
                        let
                            -- Set passed to True to avoid latency.
                            newState : PlayState
                            newState =
                                Playing { game = { game | passed = True } }
                        in
                        newState
                            ! [ send flags "end:"
                              , playSound "/sfx/endTurn.wav"
                              ]

                    PlayCard card index ->
                        let
                            -- Construct the ResolveData clientside to avoid latency.
                            newState : PlayState
                            newState =
                                resolveOutcome initial resDiffList finalState (Just state)

                            initial : Model
                            initial =
                                game.res.final

                            playDiff : Diff
                            playDiff =
                                { initDiff
                                    | turn = Just PlayerB
                                    , stack = Just <| { owner = PlayerA, card = card } :: initial.stack
                                    , hand = Just <| List.removeAt index initial.hand
                                }

                            resDiffList : List Resolvable.ResolveDiffData
                            resDiffList =
                                [ { diff = playDiff
                                  , anim = Play PlayerA card index
                                  , animDamage = ( 0, 0 )
                                  , stackCard = Nothing
                                  }
                                , { diff = initDiff
                                  , anim = Windup PlayerA
                                  , animDamage = ( 0, 0 )
                                  , stackCard = Nothing
                                  }
                                ]

                            final : Model
                            final =
                                Model.Diff.merge playDiff initial

                            finalState : PlayState
                            finalState =
                                Playing { game = Game.gameInit final }
                        in
                        newState
                            ! [ send flags <| "play:" ++ toString index
                              , playSound "/sfx/playCard.wav"
                              ]

            else
                ( state, Cmd.none )

        _ ->
            ( state, Cmd.none )


tick : Flags -> PlayState -> Float -> ( PlayState, Cmd Msg )
tick flags state dt =
    let
        game =
            get identity state

        ( newGame, msg ) =
            Game.tick flags dt game
    in
    ( set newGame state, msg )


map : (Game.Model -> Game.Model) -> PlayState -> PlayState
map f state =
    set (f <| get identity state) state


get : (Game.Model -> a) -> PlayState -> a
get g state =
    case state of
        Playing { game } ->
            g game

        Ended { game } ->
            g game


set : Game.Model -> PlayState -> PlayState
set game state =
    case state of
        Playing playing ->
            Playing { playing | game = game }

        Ended ended ->
            Ended { ended | game = game }


carry : PlayState -> PlayState -> PlayState
carry old new =
    map
        (\game ->
            { game
                | res = get .res new
                , entities = get .entities old
                , mouse = get .mouse old
                , focus = get .focus old
            }
        )
        new


resolveOutcomeStr : String -> Maybe PlayState -> PlayState
resolveOutcomeStr str mState =
    let
        resDiffList : List Resolvable.ResolveDiffData
        resDiffList =
            unsafeForceDecode
                (Json.field "list" <|
                    Json.list Resolvable.Decoders.resolveDiffData
                )
                str

        initial : Model
        initial =
            unsafeForceDecode (Json.field "initial" Model.decoder) str

        finalState : PlayState
        finalState =
            unsafeForceDecode (Json.field "final" PlayState.decoder) str
    in
    resolveOutcome initial resDiffList finalState mState


resolveOutcome : Model -> List Resolvable.ResolveDiffData -> PlayState -> Maybe PlayState -> PlayState
resolveOutcome initial resDiffList finalState mState =
    let
        state : PlayState
        state =
            Maybe.withDefault
                (Playing { game = Game.gameInit Model.init })
                mState

        oldResList : List Resolvable.ResolveData
        oldResList =
            get (.res >> .resList) state

        oldTick : Float
        oldTick =
            case oldResList of
                [] ->
                    0

                _ ->
                    get (.res >> .tick) state

        resList : List Resolvable.ResolveData
        resList =
            Resolvable.resDiffToData initial resDiffList

        model : Model
        model =
            get (.res >> .final) finalState

        res : Resolvable.Model
        res =
            { tick = oldTick
            , final = model
            , resList = oldResList ++ resList
            }

        newState : PlayState
        newState =
            map (\game -> { game | res = res }) finalState
    in
    carry state newState


mouseMove : Maybe Position -> PlayState -> PlayState
mouseMove pos state =
    let
        posToVec { x, y } =
            vec2 (toFloat x) (toFloat y)
    in
    map
        (\game -> { game | mouse = Maybe.map posToVec pos })
        state


mouseClick : Mode -> Flags -> Position -> PlayState -> ( PlayState, Cmd Main.Msg )
mouseClick mode flags { x, y } state =
    let
        pos =
            vec2 (toFloat x) (toFloat y)
    in
    case state of
        Playing { game } ->
            let
                mEntity =
                    List.find
                        (Game.hitTest pos 28)
                        game.entities.hand
            in
            case mEntity of
                Just { card, index } ->
                    update
                        (PlayingOnly <| TurnOnly <| PlayCard card index)
                        state
                        mode
                        flags

                Nothing ->
                    ( state, Cmd.none )

        _ ->
            ( state, Cmd.none )
