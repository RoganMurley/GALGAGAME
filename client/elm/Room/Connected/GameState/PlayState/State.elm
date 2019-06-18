module PlayState.State exposing (carry, get, map, mouseClick, mouseMove, resolveOutcomeStr, tick, update)

import Animation.Types exposing (Anim(..))
import Audio.State exposing (playSound)
import Browser.Navigation
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
import PlayState.Decoders as PlayState
import PlayState.Messages exposing (Msg(..), PlayingOnly(..), TurnOnly(..))
import PlayState.Types as PlayState exposing (PlayState(..), ResolveOutcomeInput)
import Ports exposing (websocketSend)
import Resolvable.State as Resolvable
import Resolvable.Types as Resolvable
import Result
import Util exposing (message)
import WhichPlayer.Types exposing (WhichPlayer(..))


update : Msg -> PlayState -> Mode -> ( PlayState, Cmd Main.Msg )
update msg state mode =
    case msg of
        PlayingOnly playingOnly ->
            updatePlayingOnly playingOnly state mode

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

        GotoComputerGame ->
            ( state, Browser.Navigation.load <| "/play/computer" )

        GotoReplay replayId ->
            ( state, Browser.Navigation.load <| "/replay/" ++ replayId )

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


updatePlayingOnly : PlayingOnly -> PlayState -> Mode.Mode -> ( PlayState, Cmd Main.Msg )
updatePlayingOnly msg state mode =
    case mode of
        Mode.Spectating ->
            ( state, Cmd.none )

        Mode.Playing ->
            case msg of
                Rematch ->
                    case state of
                        Ended _ ->
                            ( state, websocketSend "rematch:" )

                        _ ->
                            ( state, Cmd.none )

                HoverCard hover ->
                    let
                        sound =
                            case Game.getHoverIndex hover of
                                Just _ ->
                                    playSound "/sfx/hover.mp3"

                                Nothing ->
                                    Cmd.none
                    in
                    ( state
                    , Cmd.batch
                        [ message <|
                            Main.Send <|
                                "hover:"
                                    ++ Game.Encoders.encodeHoverSelf hover
                        , sound
                        ]
                    )

                IllegalPass ->
                    case state of
                        Playing { game } ->
                            let
                                -- Construct the ResolveData clientside to avoid latency.
                                newState : PlayState
                                newState =
                                    resolveOutcome
                                        (Just state)
                                        { initial = initial, resDiffList = resDiffList, finalState = state }

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
                            ( newState
                            , Cmd.none
                            )

                        _ ->
                            ( state
                            , Cmd.none
                            )

                TurnOnly turnOnly ->
                    updateTurnOnly turnOnly state


updateTurnOnly : TurnOnly -> PlayState -> ( PlayState, Cmd Main.Msg )
updateTurnOnly msg state =
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
                        ( newState
                        , Cmd.batch
                            [ websocketSend "end:"
                            , playSound "/sfx/endTurn.mp3"
                            ]
                        )

                    PlayCard card index ->
                        let
                            -- Construct the ResolveData clientside to avoid latency.
                            newState : PlayState
                            newState =
                                resolveOutcome
                                    (Just state)
                                    { initial = initial, resDiffList = resDiffList, finalState = finalState }

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
                        ( newState
                        , Cmd.batch
                            [ websocketSend <| "play:" ++ String.fromInt index
                            , playSound "/sfx/playCard.wav"
                            ]
                        )

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


resolveOutcomeStr : String -> Maybe PlayState -> Result Json.Error PlayState
resolveOutcomeStr str mState =
    Result.map (resolveOutcome mState) <|
        Json.decodeString PlayState.resolveOutcomeInputDecoder str


resolveOutcome : Maybe PlayState -> ResolveOutcomeInput -> PlayState
resolveOutcome mState { initial, resDiffList, finalState } =
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


mouseClick : Mode -> Position -> PlayState -> ( PlayState, Cmd Main.Msg )
mouseClick mode { x, y } state =
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

                Nothing ->
                    ( state, Cmd.none )

        _ ->
            ( state, Cmd.none )
