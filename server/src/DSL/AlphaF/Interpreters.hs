{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module DSL.AlphaF.Interpreters where

import Control.Monad.Freer (Eff, reinterpret, run)
import Control.Monad.Freer.State as S
import DSL.AlphaF.DSL (DSL (..), Program)
import Life (Life)
import Model (Hand, Model (..), PlayerModel (..), getPmodel, maxHandLength)
import ModelDiff (ModelDiff (..), PlayerModelDiff (..), modPmodelDiff)
import qualified ModelDiff

effI :: Model -> Program a -> (Model, a)
effI initialModel prog =
  (\(x, y) -> (y, x)) . run . runState initialModel $ reinterpret go prog
  where
    go :: DSL b -> Eff '[S.State Model] b
    go GetGen = model_gen <$> S.get
    go (GetDeck w) = pmodel_deck . getPmodel w <$> S.get
    go (GetHand w) = pmodel_hand . getPmodel w <$> S.get
    go (GetLife w) = pmodel_life . getPmodel w <$> S.get
    go (GetMaxLife w) = pmodel_maxLife . getPmodel w <$> S.get
    go GetPasses = model_passes <$> S.get
    go GetStack = model_stack <$> S.get
    go GetTurn = model_turn <$> S.get
    go GetRot = model_rot <$> S.get
    go GetHold = model_hold <$> S.get
    go GetModel = S.get
    go (SetGen g) =
      S.modify
        (\m -> ModelDiff.update m $ mempty {modeldiff_gen = Just g})
    go (SetDeck w d) =
      S.modify
        ( \m ->
            ModelDiff.update m $
              modPmodelDiff (\pm -> pm {pmodeldiff_deck = Just d}) w mempty
        )
    go (SetHand w h) =
      S.modify
        ( \m ->
            let newHand :: Hand
                newHand = take maxHandLength h
             in ModelDiff.update m $
                  modPmodelDiff (\pm -> pm {pmodeldiff_hand = Just newHand}) w mempty
        )
    go (SetLife w l) =
      S.modify
        ( \m ->
            let newLife :: Life
                newLife = max 0 l
             in ModelDiff.update m $
                  modPmodelDiff (\pm -> pm {pmodeldiff_life = Just newLife}) w mempty
        )
    go (SetMaxLife w l) =
      S.modify
        ( \m ->
            ModelDiff.update m $
              modPmodelDiff (\pm -> pm {pmodeldiff_maxLife = Just l}) w mempty
        )
    go (SetPasses p) =
      S.modify
        (\m -> ModelDiff.update m $ mempty {modeldiff_passes = Just p})
    go (SetStack s) =
      S.modify
        (\m -> ModelDiff.update m $ mempty {modeldiff_stack = Just s})
    go (SetTurn t) =
      S.modify
        (\m -> ModelDiff.update m $ mempty {modeldiff_turn = Just t})
    go (SetRot r) =
      S.modify
        (\m -> ModelDiff.update m $ mempty {modeldiff_rot = Just r})
    go (SetHold h) =
      S.modify
        (\m -> ModelDiff.update m $ mempty {modeldiff_hold = Just h})

modI :: Model -> Program () -> Model
modI m p = fst $ effI m p

evalI :: Model -> Program a -> a
evalI m p = snd $ effI m p
