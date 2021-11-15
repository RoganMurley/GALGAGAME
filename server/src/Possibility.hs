module Possibility where

import ArtificialIntelligence (Weight, evalState)
import Control.DeepSeq (rnf)
import Control.Monad.Trans.Writer (runWriter)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Debug.Trace
import GameCommand (resolveAll)
import GameState (PlayState)
import Model (Model(..))
import Player (WhichPlayer)
import Util (Gen, infiniteGens)
import Wheel (Wheel(..))

import qualified Replay.Active as Replay

import qualified DSL.Alpha as Alpha
import qualified DSL.Beta as Beta
import DSL.Beta


tipTheOdds :: WhichPlayer -> Beta.Program ()
tipTheOdds w = do
    gen <- getGen
    initialModel <- getModel
    let model = Alpha.modI initialModel (Alpha.modStack (\stack -> stack { wheel_0 = Nothing }))
    _ <- trace (show model) (return ())
    let gens = take 5 (infiniteGens gen) :: [Gen]
    _ <- return $ rnf gens
    let models = fmap (\g -> model { model_gen = g }) gens :: [Model]
    _ <- return $ rnf models
    let states = fmap (\m -> fst . runWriter $ resolveAll m Replay.null 0) models :: [PlayState]
    _ <- return $ rnf states
    let weights = fmap (evalState w) states :: [Weight]
    _ <- return $ rnf weights
    let (_, i) = maximumBy (comparing fst) (zip weights [0..])
    let newGen = gens !! i -- To avoid a space leak
    raw (Alpha.setGen newGen)
    trace "tipped" (return ())
