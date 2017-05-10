module GameState.Types exposing (GameState(..), fullify, unfullify)

import CharacterSelect.Types as CharacterSelect
import Model.Types exposing (FullModel, Model, ModelDiff, Res, WhichPlayer)


type GameState
    = Waiting
    | Selecting CharacterSelect.Model
    | PlayingGame FullModel ( Res, Int )
    | Ended (Maybe WhichPlayer) (Maybe FullModel) ( Res, Int )


fullify : Model -> ModelDiff {} -> FullModel
fullify { hand, otherHand, stack, turn, life, otherLife, otherHover } { diffOtherLife, diffLife } =
    { hand = hand
    , otherHand = otherHand
    , stack = stack
    , turn = turn
    , life = life
    , otherLife = otherLife
    , otherHover = otherHover
    , diffOtherLife = diffOtherLife
    , diffLife = diffLife
    }


unfullify : FullModel -> Model
unfullify { hand, otherHand, stack, turn, life, otherLife, otherHover } =
    { hand = hand
    , otherHand = otherHand
    , stack = stack
    , turn = turn
    , life = life
    , otherLife = otherLife
    , otherHover = otherHover
    }
