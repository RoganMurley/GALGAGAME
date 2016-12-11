module GameState exposing (Card, GameState(..), Hand, Model, Turn, WhichPlayer(..), init, resTick, stateUpdate, stateView, tickForward, tickZero, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (field, maybe)
import Messages exposing (GameMsg(..), Msg(DrawCard, EndTurn, HoverCard, PlayCard, Rematch))


-- TYPES.


type GameState
    = Waiting
    | PlayingGame Model ( Res, Int )
    | Ended (Maybe WhichPlayer) ( Res, Int )


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


type alias Card =
    { name : String
    , desc : String
    , imgURL : String
    }


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


stateView : GameState -> Html Msg
stateView state =
    case state of
        Waiting ->
            div [ class "waiting" ] [ text "Waiting for opponent..." ]

        PlayingGame model ( res, _ ) ->
            case res of
                [] ->
                    view model

                otherwise ->
                    resView res model

        Ended winner ( res, _ ) ->
            case (List.head res) of
                Just r ->
                    resView res r

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


view : Model -> Html Msg
view model =
    div []
        [ viewOtherHand model.otherHand model.otherHover
        , viewHand model.hand
        , viewStack model.stack
        , viewTurn (List.length model.hand == maxHandLength) model.turn
        , viewLife PlayerA model.life
        , viewLife PlayerB model.otherLife
        ]


viewHand : Hand -> Html Msg
viewHand hand =
    let
        viewCard : Card -> Html Msg
        viewCard { name, desc, imgURL } =
            div
                [ class "card my-card"
                , onClick (PlayCard name)
                , onMouseEnter (HoverCard (Just name))
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
        div [ class "hand my-hand" ] (List.map viewCard hand)


viewOtherHand : Int -> HoverCardIndex -> Html Msg
viewOtherHand cardCount hoverIndex =
    let
        viewCard : Int -> Html Msg
        viewCard index =
            case (Just index) == hoverIndex of
                True ->
                    div [ class "card other-card other-card-hover" ] []

                False ->
                    div [ class "card other-card" ] []

        cards : List (Html Msg)
        cards =
            List.map viewCard (List.range 0 (cardCount - 1))
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
        viewCard : Card -> Html Msg
        viewCard { name, desc, imgURL } =
            div
                [ class "card"
                ]
                [ div [ class "card-title" ] [ text name ]
                , div
                    [ class "card-picture"
                    , style [ ( "background-image", "url(\"img/" ++ imgURL ++ "\")" ) ]
                    ]
                    []
                , div [ class "card-desc" ] [ text desc ]
                ]

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
            resProcess state (syncState state str)

        HoverOutcome i ->
            case state of
                PlayingGame m r ->
                    PlayingGame { m | otherHover = i } r

                s ->
                    s


syncState : GameState -> String -> GameState
syncState model msg =
    decodeState msg


decodeState : String -> GameState
decodeState msg =
    case decodePlaying msg of
        Ok playingState ->
            playingState

        Err err1 ->
            case decodeWaiting msg of
                Ok waitingState ->
                    waitingState

                Err err2 ->
                    case decodeEnded msg of
                        Ok drawState ->
                            drawState

                        Err err3 ->
                            Debug.crash
                                ("Error 1:\n"
                                    ++ err1
                                    ++ "\nError 2:\n"
                                    ++ err2
                                    ++ "\nError 3:\n"
                                    ++ err3
                                )


decodeWaiting : String -> Result String GameState
decodeWaiting msg =
    let
        decoder : Json.Decoder GameState
        decoder =
            Json.map (\_ -> Waiting) (field "waiting" Json.bool)
    in
        Json.decodeString decoder msg


decodeEnded : String -> Result String GameState
decodeEnded msg =
    let
        decoder : Json.Decoder GameState
        decoder =
            Json.map2 (\w res -> Ended w ( res, 0 ))
                (field "winner" (maybe whichDecoder))
                resDecoder
    in
        Json.decodeString decoder msg


decodePlaying : String -> Result String GameState
decodePlaying msg =
    let
        decoder : Json.Decoder GameState
        decoder =
            Json.map2 (\a b -> PlayingGame a ( b, 0 ))
                (field "playing" modelDecoder)
                (field "playing" resDecoder)
    in
        Json.decodeString decoder msg


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


modelDecoder : Json.Decoder Model
modelDecoder =
    let
        cardDecoder : Json.Decoder Card
        cardDecoder =
            Json.map3 Card
                (field "name" Json.string)
                (field "desc" Json.string)
                (field "imageURL" Json.string)

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


resDecoder : Json.Decoder (List Model)
resDecoder =
    field "res" (Json.list modelDecoder)



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


resProcess : GameState -> GameState -> GameState
resProcess old new =
    case new of
        PlayingGame _ ( [], _ ) ->
            new

        otherwise ->
            case ( old, new ) of
                ( PlayingGame oldModel _, PlayingGame model ( res, _ ) ) ->
                    PlayingGame oldModel ( res ++ [ model ], 0 )

                otherwise ->
                    new


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


resView : Res -> Model -> Html Msg
resView res model =
    div []
        [ viewOtherHand model.otherHand model.otherHover
        , viewResHand model.hand
        , viewStack model.stack
        , viewResTurn
        , viewLife PlayerA model.life
        , viewLife PlayerB model.otherLife
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
