module GameState exposing (Card, GameState(..), Hand, Model, Turn, WhichPlayer(..), init, resTick, stateUpdate, stateView, tickForward, tickZero, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (field)
import Messages exposing (GameMsg(Sync), Msg(DrawCard, EndTurn, PlayCard, Rematch))


-- TYPES.


type GameState
    = Waiting
    | PlayingGame Model ( Res, Int )
    | Victory WhichPlayer
    | Draw


type alias Model =
    { hand : Hand
    , otherHand : Int
    , stack : Stack
    , turn : Turn
    , life : Life
    , otherLife : Life
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



-- INITIAL MODEL.


init : Model
init =
    { hand = []
    , otherHand = 0
    , stack = []
    , turn = PlayerA
    , life = 1000
    , otherLife = 1000
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

        Victory PlayerA ->
            div [ class "endgame" ]
                [ div [ class "victory" ] [ text "VICTORY" ]
                , button [ class "rematch", onClick Rematch ] [ text "Rematch" ]
                ]

        Victory PlayerB ->
            div [ class "endgame" ]
                [ div [ class "defeat" ] [ text "DEFEAT" ]
                , button [ class "rematch", onClick Rematch ] [ text "Rematch" ]
                ]

        Draw ->
            div [ class "endgame" ]
                [ div [ class "draw" ] [ text "DRAW" ]
                , button [ class "rematch", onClick Rematch ] [ text "Rematch" ]
                ]


view : Model -> Html Msg
view model =
    div []
        [ viewOtherHand model.otherHand
        , viewHand model.hand
        , viewStack model.stack
        , viewTurn model.turn
        , viewLife ( model.life, model.otherLife )
        ]


viewHand : Hand -> Html Msg
viewHand hand =
    let
        viewCard : Card -> Html Msg
        viewCard { name, desc, imgURL } =
            div
                [ class "card my-card"
                , onClick (PlayCard name)
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


viewOtherHand : Int -> Html Msg
viewOtherHand cardCount =
    let
        viewCard : Html Msg
        viewCard =
            div [ class "card other-card" ] []
    in
        div [ class "hand other-hand" ] (List.repeat cardCount viewCard)


viewTurn : Turn -> Html Msg
viewTurn turn =
    case turn of
        PlayerA ->
            button [ class "turn-indi pass-button", onClick EndTurn ] [ text "Pass" ]

        PlayerB ->
            div [ class "turn-indi enemy-turn" ] [ text "Opponent's Turn" ]


viewLife : ( Life, Life ) -> Html Msg
viewLife ( myLife, otherLife ) =
    div
        [ class "life" ]
        [ div [ class "life-counter" ] [ text ("Opponent HP: " ++ (toString otherLife)) ]
        , div [ class "life-counter" ] [ text ("Your HP: " ++ (toString myLife)) ]
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
                    div [ class "playera" ] [ viewCard card ]

                PlayerB ->
                    div [ class "playerb" ] [ viewCard card ]
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
                    case decodeVictory msg of
                        Ok victoryState ->
                            victoryState

                        Err err3 ->
                            case decodeDraw msg of
                                Ok drawState ->
                                    drawState

                                Err err4 ->
                                    Debug.crash
                                        ("Error 1:\n"
                                            ++ err1
                                            ++ "\nError 2:\n"
                                            ++ err2
                                            ++ "\nError 3:\n"
                                            ++ err3
                                            ++ "\nError 4:\n"
                                            ++ err4
                                        )


decodeWaiting : String -> Result String GameState
decodeWaiting msg =
    let
        decoder : Json.Decoder GameState
        decoder =
            Json.map (\_ -> Waiting) (field "waiting" Json.bool)
    in
        Json.decodeString decoder msg


decodeVictory : String -> Result String GameState
decodeVictory msg =
    let
        decoder : Json.Decoder GameState
        decoder =
            Json.map Victory (field "victory" whichDecoder)
    in
        Json.decodeString decoder msg


decodeDraw : String -> Result String GameState
decodeDraw msg =
    let
        decoder : Json.Decoder GameState
        decoder =
            Json.map (\_ -> Draw) (field "draw" Json.bool)
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
        Json.map6 Model
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

        otherwise ->
            state


tickZero : GameState -> Bool
tickZero state =
    case state of
        PlayingGame _ ( _, 0 ) ->
            True

        otherwise ->
            False


resView : Res -> Model -> Html Msg
resView res model =
    div []
        [ viewOtherHand model.otherHand
        , viewResHand model.hand
        , viewStack model.stack
        , viewResTurn
        , viewLife ( model.life, model.otherLife )
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
