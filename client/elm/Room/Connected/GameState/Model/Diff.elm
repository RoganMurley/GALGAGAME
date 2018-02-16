module Model.Diff exposing (..)

import Card.Decoders as Card
import Json.Decode as Json exposing (Decoder, field, index, int, list, maybe, string)
import Model.Types exposing (Life, Model)
import Hand.Types exposing (Hand)
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
        (field "handPA" <| maybe <| list Card.decoder)
        (field "handPB" <| maybe int)
        (field "stack" <| maybe <| list Stack.stackCardDecoder)
        (field "turn" <| maybe WhichPlayer.decoder)
        (field "lifePA" <| maybe int)
        (field "lifePB" <| maybe int)


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
