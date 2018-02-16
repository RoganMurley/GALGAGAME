module DSL.Alpha.Interpreters where

import Control.Monad.Free (Free(..), liftF)
import Data.Functor.Sum (Sum(..))
import DSL.Alpha.DSL
import DSL.Util (toLeft, toRight)
import Model
import ModelDiff (PlayerModelDiff(..), ModelDiff(..), modPmodelDiff)
import Text.Printf (printf)

import qualified DSL.Log as Log


effI :: Model -> Program a -> (Model, a)
effI m (Pure x) = (m, x)
effI m (Free p) = (uncurry effI) (alphaEffI m p)


modI :: Model -> Program () -> Model
modI m p = fst $ effI m p


evalI :: Model -> Program a -> a
evalI m p = snd $ effI m p


alphaEffI :: Model -> DSL a -> (Model, a)
alphaEffI m (GetGen f)      = (m, f $ model_gen m)
alphaEffI m (GetPasses f)   = (m, f $ model_passes m)
alphaEffI m (GetStack f)    = (m, f $ model_stack m)
alphaEffI m (GetTurn f)     = (m, f $ model_turn m)
alphaEffI m (GetDeck w f)   = (m, f . pmodel_deck $ getPmodel w m)
alphaEffI m (GetHand w f)   = (m, f . pmodel_hand $ getPmodel w m )
alphaEffI m (GetLife w f)   = (m, f . pmodel_life $ getPmodel w m)
alphaEffI m (SetGen g n)    = (m { model_gen = g }, n)
alphaEffI m (SetDeck w d n) = (modPmodel (\pm -> pm { pmodel_deck = d }) w m, n)
alphaEffI m (SetHand w h n) =
                              let
                                newHand :: Hand
                                newHand = take maxHandLength h
                              in
                                (modPmodel (\pm -> pm { pmodel_hand = newHand }) w m, n)
alphaEffI m (SetLife w l n) =
                              let
                                newLife :: Life
                                newLife = max 0 . min maxLife $ l
                              in
                                (modPmodel (\pm -> pm { pmodel_life = newLife }) w m, n)
alphaEffI m (SetPasses p n) = (m { model_passes = p }, n)
alphaEffI m (SetStack s n)  = (m { model_stack = s }, n)
alphaEffI m (SetTurn t n)   = (m { model_turn = t }, n)


logI :: DSL a -> Log.Program ()
logI (GetGen _)      = Log.log $ printf "Get gen"
logI (GetDeck w _)   = Log.log $ printf "Get deck %s" (show w)
logI (GetHand w _)   = Log.log $ printf "Get hand %s" (show w)
logI (GetLife w _)   = Log.log $ printf "Get life %s" (show w)
logI (GetPasses _)   = Log.log $ printf "Get passes"
logI (GetStack _)    = Log.log $ printf "Get stack"
logI (GetTurn _)     = Log.log $ printf "Get turn"
logI (SetGen g _)    = Log.log $ printf "Set gen %s"     (show g)
logI (SetDeck w d _) = Log.log $ printf "Set deck %s %s" (show w) (show d)
logI (SetHand w h _) = Log.log $ printf "Set hand %s %s" (show w) (show h)
logI (SetLife w l _) = Log.log $ printf "Set life %s %s" (show w) (show l)
logI (SetPasses p _) = Log.log $ printf "Set passes %s"  (show p)
logI (SetStack s _)  = Log.log $ printf "Set stack %s"   (show s)
logI (SetTurn t _)   = Log.log $ printf "Set turn %s"    (show t)


decorateLog :: ∀ a . DSL a -> Free (Sum DSL Log.DSL) a
decorateLog x =
  let
    alpha   = liftF x :: Program a
    logging = logI x  :: Log.Program ()
  in
    toLeft alpha <* toRight logging


diffI :: DSL a -> ModelDiff -> ModelDiff
diffI (SetGen g _)    diff  = diff { modeldiff_gen = Just g }
diffI (SetDeck w d _) diff  = modPmodelDiff (\pm -> pm { pmodeldiff_deck = Just d }) w diff
diffI (SetHand w h _) diff  = modPmodelDiff (\pm -> pm { pmodeldiff_hand = Just h }) w diff
diffI (SetLife w l _) diff  = modPmodelDiff (\pm -> pm { pmodeldiff_life = Just l }) w diff
diffI (SetPasses p _) diff  = diff { modeldiff_passes = Just p }
diffI (SetStack s _)  diff  = diff { modeldiff_stack = Just s }
diffI (SetTurn t _)   diff  = diff { modeldiff_turn = Just t }
diffI (GetGen _)      diff  = diff
diffI (GetDeck _ _)   diff  = diff
diffI (GetHand _ _)   diff  = diff
diffI (GetLife _ _)   diff  = diff
diffI (GetPasses _)   diff  = diff
diffI (GetStack _)    diff  = diff
diffI (GetTurn _)     diff  = diff
