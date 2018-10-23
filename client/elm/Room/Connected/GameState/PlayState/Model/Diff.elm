module Model.Diff exposing (Diff, decoder, initDiff, merge)

import Card.Decoders as Card
import Hand.Types exposing (Hand)
import Json.Decode as Json exposing (Decoder, field, int, list, maybe)
import Model.Types exposing (Life, Model)
import Stack.Decoders as Stack
import Stack.Types exposing (Stack)
import WhichPlayer.Decoders as WhichPlayer
import WhichPlayer.Types exposing (WhichPlayer)


type alias Diff =
    { hand : Maybe Hand
    , otherHand : Maybe Int
    , stack : Maybe Stack
    , turn : Maybe WhichPlayer
    , life : Maybe Life
    , otherLife : Maybe Life
    }


decoder : Decoder Diff
decoder =
    Json.map6 Diff
        (maybe <| field "handPA" <| list Card.decoder)
        (maybe <| field "handPB" int)
        (maybe <| field "stack" <| list Stack.stackCardDecoder)
        (maybe <| field "turn" WhichPlayer.decoder)
        (maybe <| field "lifePA" int)
        (maybe <| field "lifePB" int)


merge : Diff -> Model -> Model
merge diff model =
    { model
        | hand = Maybe.withDefault model.hand diff.hand
        , otherHand = Maybe.withDefault model.otherHand diff.otherHand
        , stack = Maybe.withDefault model.stack diff.stack
        , turn = Maybe.withDefault model.turn diff.turn
        , life = Maybe.withDefault model.life diff.life
        , otherLife = Maybe.withDefault model.otherLife diff.otherLife
    }


initDiff : Diff
initDiff =
    { hand = Nothing
    , otherHand = Nothing
    , stack = Nothing
    , turn = Nothing
    , life = Nothing
    , otherLife = Nothing
    }
