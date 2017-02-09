module GameState exposing (GameState(..), Hand, Model, Turn, WhichPlayer(..), init, resTick, stateUpdate, stateView, tickForward, tickZero, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (field, maybe)
import Card exposing (Card, viewCard)
import CharacterSelect
import Messages exposing (GameMsg(..), Msg(CopyInput, DrawCard, EndTurn, HoverCard, PlayCard, Rematch, SelectAllInput))
import Util exposing (fromJust)
import Vfx


type GameState
    = Waiting
    | Selecting CharacterSelect.Model
    | PlayingGame Model ( Res, Int )
    | Ended (Maybe WhichPlayer) ( Res, Int )


setRes : GameState -> List Model -> GameState
setRes state res =
    case state of
        PlayingGame m ( _, i ) ->
            PlayingGame m ( res, i )

        Ended w ( _, i ) ->
            Ended w ( res, i )

        Waiting ->
            Debug.crash "Set res on a waiting state"

        Selecting _ ->
            Debug.crash "Set res on a Selecting state"


type alias Model =
    { hand : Hand
    , otherHand : Int
    , stack : Stack
    , turn : Turn
    , life : Life
    , otherLife : Life
    , otherHover : Maybe Int
    }


type alias Hand =
    List Card


type alias Res =
    List Model


type alias Stack =
    List StackCard


type WhichPlayer
    = PlayerA
    | PlayerB


type alias Turn =
    WhichPlayer


type alias StackCard =
    { owner : WhichPlayer
    , card : Card
    }


type alias Life =
    Int


type alias HoverCardIndex =
    Maybe Int



-- INITIAL MODEL.


maxHandLength : Int
maxHandLength =
    6


init : Model
init =
    { hand = []
    , otherHand = 0
    , stack = []
    , turn = PlayerA
    , life = 100
    , otherLife = 100
    , otherHover = Nothing
    }



-- VIEWS.


stateView : GameState -> String -> String -> String -> Float -> ( Int, Int ) -> Html Msg
stateView state roomID hostname httpPort time ( width, height ) =
    let
        params =
            Vfx.Params time ( width, height )
    in
        case state of
            Waiting ->
                let
                    portProtocol =
                        if httpPort /= "" then
                            ":" ++ httpPort
                        else
                            ""

                    challengeLink =
                        "http://" ++ hostname ++ portProtocol ++ "?play=" ++ roomID

                    myID =
                        "challenge-link"
                in
                    div [ class "waiting" ]
                        [ div [ class "waiting-prompt" ] [ text "Give this link to your opponent:" ]
                        , div [ class "input-group" ]
                            [ input [ value challengeLink, type_ "text", readonly True, id myID, onClick (SelectAllInput myID) ] []
                            , button [ onClick (CopyInput myID) ] [ text "copy" ]
                            ]
                        ]

            Selecting model ->
                CharacterSelect.view model

            PlayingGame model ( res, _ ) ->
                case res of
                    [] ->
                        view params 0.0 model

                    otherwise ->
                        resView params 1.0 res model

            Ended winner ( res, _ ) ->
                case (List.head res) of
                    Just r ->
                        resView params 1.0 res r

                    Nothing ->
                        div [ class "endgame" ]
                            (case winner of
                                Nothing ->
                                    [ div [ class "draw" ] [ text "DRAW" ]
                                    , button [ class "rematch", onClick Rematch ] [ text "Rematch" ]
                                    ]

                                Just player ->
                                    [ if player == PlayerA then
                                        div [ class "victory" ] [ text "VICTORY" ]
                                      else
                                        div [ class "defeat" ] [ text "DEFEAT" ]
                                    , button [ class "rematch", onClick Rematch ] [ text "Rematch" ]
                                    ]
                            )


view : Vfx.Params -> Float -> Model -> Html Msg
view params intensity model =
    div []
        [ viewOtherHand model.otherHand model.otherHover
        , viewHand model.hand
        , viewStack model.stack
        , viewTurn (List.length model.hand == maxHandLength) model.turn
        , viewLife PlayerA model.life
        , viewLife PlayerB model.otherLife
        , Vfx.view params intensity
        ]


viewHand : Hand -> Html Msg
viewHand hand =
    let
        viewCard : ( Int, Card ) -> Html Msg
        viewCard ( index, { name, desc, imgURL } ) =
            div
                [ class "card my-card"
                , onClick (PlayCard index)
                , onMouseEnter (HoverCard (Just index))
                , onMouseLeave (HoverCard Nothing)
                ]
                [ div [ class "card-title" ] [ text name ]
                , div
                    [ class "card-picture"
                    , style [ ( "background-image", "url(\"img/" ++ imgURL ++ "\")" ) ]
                    ]
                    []
                , div [ class "card-desc" ] [ text desc ]
                ]
    in
        div [ class "hand my-hand" ] (List.map viewCard (List.indexedMap (,) hand))


viewOtherHand : Int -> HoverCardIndex -> Html Msg
viewOtherHand cardCount hoverIndex =
    let
        viewCard : Int -> Html Msg
        viewCard index =
            div [ containerClass index hoverIndex ]
                [ div
                    [ class "card other-card"
                    , style [ ( "transform", "rotateZ(" ++ toString (calcRot index) ++ "deg) translateY(" ++ toString (calcTrans index) ++ "px)" ) ]
                    ]
                    []
                ]

        -- Stupid container nesting because css transform overwrite.
        containerClass : Int -> HoverCardIndex -> Attribute msg
        containerClass index hoverIndex =
            case hoverIndex of
                Just i ->
                    if i == index then
                        class "other-card-container card-hover"
                    else
                        class "other-card-container"

                Nothing ->
                    class "other-card-container"

        cards : List (Html Msg)
        cards =
            List.map viewCard (List.range 0 (cardCount - 1))

        calcRot : Int -> Int
        calcRot index =
            -2 * (index - (cardCount // 2))

        calcTrans : Int -> Int
        calcTrans index =
            -12 * (abs (index - (cardCount // 2)))
    in
        div [ class "hand other-hand" ] cards


viewTurn : Bool -> Turn -> Html Msg
viewTurn handFull turn =
    case turn of
        PlayerA ->
            case handFull of
                False ->
                    button [ class "turn-indi pass-button", onClick EndTurn ] [ text "Pass" ]

                True ->
                    button [ class "turn-indi pass-button pass-disabled" ] [ text "Hand full" ]

        PlayerB ->
            div [ class "turn-indi enemy-turn" ] [ text "Opponent's Turn" ]


viewLife : WhichPlayer -> Life -> Html Msg
viewLife which life =
    let
        barWidth : Life -> String
        barWidth barLife =
            (toString (((toFloat barLife) / 50) * 100)) ++ "%"

        whoseLife : String
        whoseLife =
            case which of
                PlayerA ->
                    "life-mine"

                PlayerB ->
                    ""
    in
        div
            [ class "life", class whoseLife ]
            [ div
                [ class "life-bar" ]
                [ div [ class "life-text" ] [ text ("♥ " ++ (toString life) ++ " ♥") ]
                , div [ class "life-health", style [ ( "width", barWidth life ) ] ] []
                ]
            ]


viewStack : Stack -> Html Msg
viewStack stack =
    let
        viewStackCard : StackCard -> Html Msg
        viewStackCard { owner, card } =
            case owner of
                PlayerA ->
                    div [ class "playera stack-card" ] [ viewCard card ]

                PlayerB ->
                    div [ class "playerb stack-card" ] [ viewCard card ]
    in
        div
            [ class "stack-container" ]
            [ div [ class "stack" ] (List.map viewStackCard stack)
            ]



-- UPDATE


stateUpdate : GameMsg -> GameState -> GameState
stateUpdate msg state =
    case msg of
        Sync str ->
            syncState state str

        HoverOutcome i ->
            case state of
                PlayingGame m r ->
                    PlayingGame { m | otherHover = i } r

                s ->
                    s

        ResolveOutcome str ->
            let
                ( final, resList ) =
                    case Json.decodeString (resDecoder state) str of
                        Ok result ->
                            result

                        Err err ->
                            Debug.crash err
            in
                case resList of
                    [] ->
                        setRes final []

                    otherwise ->
                        case ( state, final ) of
                            ( PlayingGame oldModel _, PlayingGame newModel _ ) ->
                                PlayingGame oldModel ( resList ++ [ newModel ], 0 )

                            otherwise ->
                                setRes final resList

        SelectingMsg selectMsg ->
            let
                model : CharacterSelect.Model
                model =
                    case state of
                        Selecting m ->
                            m

                        otherwise ->
                            Debug.crash "Expected a selecting state"
            in
                Selecting (CharacterSelect.update selectMsg model)


syncState : GameState -> String -> GameState
syncState oldState msg =
    decodeState msg oldState


decodeState : String -> GameState -> GameState
decodeState msg oldState =
    case Json.decodeString (stateDecoder oldState) msg of
        Ok result ->
            result

        Err err ->
            Debug.crash err



-- Make safer


stateDecoder : GameState -> Json.Decoder GameState
stateDecoder oldState =
    Json.oneOf
        [ waitingDecoder
        , selectingDecoder oldState
        , playingDecoder
        , endedDecoder
        ]


waitingDecoder : Json.Decoder GameState
waitingDecoder =
    Json.map (\_ -> Waiting) (field "waiting" Json.bool)


selectingDecoder : GameState -> Json.Decoder GameState
selectingDecoder oldState =
    let
        characterDecoder : Json.Decoder CharacterSelect.Character
        characterDecoder =
            Json.map2 CharacterSelect.Character
                (field "name" Json.string)
                (field "cards" characterCardsDecoder)

        characterCardsDecoder : Json.Decoder ( Card, Card, Card, Card )
        characterCardsDecoder =
            Json.map4 (,,,)
                (Json.index 0 cardDecoder)
                (Json.index 1 cardDecoder)
                (Json.index 2 cardDecoder)
                (Json.index 3 cardDecoder)

        makeSelectState : List CharacterSelect.Character -> List CharacterSelect.Character -> GameState
        makeSelectState selecting selected =
            Selecting (CharacterSelect.Model selecting (toSelection selected) (hoverCharacter (fromJust (List.head selecting))))

        toSelection : List CharacterSelect.Character -> CharacterSelect.SelectedCharacters
        toSelection cs =
            case cs of
                [] ->
                    CharacterSelect.NoneSelected

                [ a ] ->
                    CharacterSelect.OneSelected a.name

                [ a, b ] ->
                    CharacterSelect.TwoSelected a.name b.name

                [ a, b, c ] ->
                    CharacterSelect.ThreeSelected a.name b.name c.name

                otherwise ->
                    CharacterSelect.NoneSelected

        hoverCharacter : CharacterSelect.Character -> CharacterSelect.Character
        hoverCharacter default =
            case oldState of
                Selecting { hover } ->
                    hover

                otherwise ->
                    default
    in
        Json.map2 makeSelectState
            (field "selecting" (Json.list characterDecoder))
            (field "selected" (Json.list characterDecoder))


endedDecoder : Json.Decoder GameState
endedDecoder =
    Json.map (\w -> Ended w ( [], 0 ))
        (field "winner" (maybe whichDecoder))


playingDecoder : Json.Decoder GameState
playingDecoder =
    Json.map (\a -> PlayingGame a ( [], 0 ))
        (field "playing" modelDecoder)


whichDecoder : Json.Decoder WhichPlayer
whichDecoder =
    let
        makeWhich : String -> WhichPlayer
        makeWhich s =
            case s of
                "pa" ->
                    PlayerA

                "pb" ->
                    PlayerB

                otherwise ->
                    Debug.crash ("Invalid player " ++ s)
    in
        Json.map makeWhich Json.string


cardDecoder : Json.Decoder Card
cardDecoder =
    Json.map3 Card
        (field "name" Json.string)
        (field "desc" Json.string)
        (field "imageURL" Json.string)


modelDecoder : Json.Decoder Model
modelDecoder =
    let
        stackCardDecoder : Json.Decoder StackCard
        stackCardDecoder =
            Json.map2 StackCard
                (field "owner" whichDecoder)
                (field "card" cardDecoder)
    in
        Json.map6 (\a b c d e f -> Model a b c d e f Nothing)
            (field "handPA" (Json.list cardDecoder))
            (field "handPB" Json.int)
            (field "stack" (Json.list stackCardDecoder))
            (field "turn" whichDecoder)
            (field "lifePA" Json.int)
            (field "lifePB" Json.int)


resDecoder : GameState -> Json.Decoder ( GameState, List Model )
resDecoder oldState =
    Json.map2 (\x y -> ( x, y ))
        (field "final" (stateDecoder oldState))
        (field "list" (Json.list modelDecoder))



-- RESOLVING.


resDelay : Int
resDelay =
    30


resTick : GameState -> GameState
resTick state =
    let
        safeTail : List a -> List a
        safeTail l =
            case List.tail l of
                Just t ->
                    t

                Nothing ->
                    []
    in
        case state of
            PlayingGame model ( res, _ ) ->
                case List.head res of
                    Just newModel ->
                        PlayingGame newModel ( safeTail res, resDelay )

                    Nothing ->
                        PlayingGame model ( res, 0 )

            Ended p ( res, _ ) ->
                Ended p ( List.drop 1 res, resDelay )

            otherwise ->
                state


tickForward : GameState -> GameState
tickForward state =
    case state of
        PlayingGame model ( res, tick ) ->
            PlayingGame model ( res, tick - 1 )

        Ended p ( res, tick ) ->
            Ended p ( res, tick - 1 )

        otherwise ->
            state


tickZero : GameState -> Bool
tickZero state =
    case state of
        PlayingGame _ ( _, 0 ) ->
            True

        Ended _ ( _, 0 ) ->
            True

        otherwise ->
            False


resView : Vfx.Params -> Float -> Res -> Model -> Html Msg
resView params intensity res model =
    div []
        [ viewOtherHand model.otherHand model.otherHover
        , viewResHand model.hand
        , viewStack model.stack
        , viewResTurn
        , viewLife PlayerA model.life
        , viewLife PlayerB model.otherLife
        , Vfx.view params intensity
        ]


viewResHand : Hand -> Html Msg
viewResHand hand =
    let
        viewCard : Card -> Html Msg
        viewCard { name, desc, imgURL } =
            div
                [ class "card my-card"
                ]
                [ div [ class "card-title" ] [ text name ]
                , div
                    [ class "card-picture"
                    , style [ ( "background-image", "url(\"img/" ++ imgURL ++ "\")" ) ]
                    ]
                    []
                , div [ class "card-desc" ] [ text desc ]
                ]
    in
        div [ class "hand my-hand" ] (List.map viewCard hand)


viewResTurn : Html Msg
viewResTurn =
    div [ class "turn-indi" ] [ text "Resolving..." ]
