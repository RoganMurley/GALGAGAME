{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module DSL.Alpha.Interpreters where

import Control.Monad.Freer (Eff, reinterpret, run)
import Control.Monad.Freer.State as S
import DSL.Alpha.DSL (DSL (..), Program)
import Life (Life)
import Model (Hand, Model (..), PlayerModel (..), getPmodel, maxHandLength)
import ModelDiff (ModelDiff (..), PlayerModelDiff (..), modPmodelDiff)
import qualified ModelDiff

effI :: Model -> Program a -> (Model, a)
effI initialModel prog =
  (\(x, y) -> (y, x)) . run . runState initialModel $ reinterpret go prog
  where
    go :: DSL b -> Eff '[S.State Model] b
    go dsl = do
      m <- S.get
      let (diff, result) = alphaEffI m dsl
      S.put (ModelDiff.update m diff)
      return result

alphaEffI :: Model -> DSL a -> (ModelDiff, a)
alphaEffI m GetGen = (mempty, model_gen m)
alphaEffI m GetPasses = (mempty, model_passes m)
alphaEffI m GetStack = (mempty, model_stack m)
alphaEffI m GetTurn = (mempty, model_turn m)
alphaEffI m GetRot = (mempty, model_rot m)
alphaEffI m GetHold = (mempty, model_hold m)
alphaEffI m (GetDeck w) = (mempty, pmodel_deck $ getPmodel w m)
alphaEffI m (GetHand w) = (mempty, pmodel_hand $ getPmodel w m)
alphaEffI m (GetLife w) = (mempty, pmodel_life $ getPmodel w m)
alphaEffI m (GetMaxLife w) = (mempty, pmodel_maxLife $ getPmodel w m)
alphaEffI m GetModel = (mempty, m)
alphaEffI _ dsl@(SetGen _) = (diffI dsl mempty, ())
alphaEffI _ dsl@(SetDeck _ _) = (diffI dsl mempty, ())
alphaEffI _ dsl@(SetHand _ _) = (diffI dsl mempty, ())
alphaEffI _ dsl@(SetLife _ _) = (diffI dsl mempty, ())
alphaEffI _ dsl@(SetMaxLife _ _) = (diffI dsl mempty, ())
alphaEffI _ dsl@(SetPasses _) = (diffI dsl mempty, ())
alphaEffI _ dsl@(SetStack _) = (diffI dsl mempty, ())
alphaEffI _ dsl@(SetTurn _) = (diffI dsl mempty, ())
alphaEffI _ dsl@(SetRot _) = (diffI dsl mempty, ())
alphaEffI _ dsl@(SetHold _) = (diffI dsl mempty, ())

modI :: Model -> Program () -> Model
modI m p = fst $ effI m p

evalI :: Model -> Program a -> a
evalI m p = snd $ effI m p

diffI :: DSL a -> ModelDiff -> ModelDiff
diffI (SetGen g) diff = diff {modeldiff_gen = Just g}
diffI (SetDeck w d) diff = modPmodelDiff (\pm -> pm {pmodeldiff_deck = Just d}) w diff
diffI (SetHand w h) diff =
  let newHand :: Hand
      newHand = take maxHandLength h
   in modPmodelDiff (\pm -> pm {pmodeldiff_hand = Just newHand}) w diff
diffI (SetLife w l) diff =
  let newLife :: Life
      newLife = max 0 l
   in modPmodelDiff (\pm -> pm {pmodeldiff_life = Just newLife}) w diff
diffI (SetMaxLife w l) diff = modPmodelDiff (\pm -> pm {pmodeldiff_maxLife = Just l}) w diff
diffI (SetPasses p) diff = diff {modeldiff_passes = Just p}
diffI (SetStack s) diff = diff {modeldiff_stack = Just s}
diffI (SetTurn t) diff = diff {modeldiff_turn = Just t}
diffI (SetRot r) diff = diff {modeldiff_rot = Just r}
diffI (SetHold h) diff = diff {modeldiff_hold = Just h}
diffI GetGen diff = diff
diffI (GetDeck _) diff = diff
diffI (GetHand _) diff = diff
diffI (GetLife _) diff = diff
diffI (GetMaxLife _) diff = diff
diffI GetPasses diff = diff
diffI GetStack diff = diff
diffI GetTurn diff = diff
diffI GetRot diff = diff
diffI GetHold diff = diff
diffI GetModel diff = diff
