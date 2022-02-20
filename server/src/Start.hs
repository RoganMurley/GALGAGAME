module Start where

import qualified Cards
import Control.Monad (replicateM_)
import qualified DSL.Alpha as Alpha
import qualified DSL.Beta as Beta
import Model (Turn, maxHandLength)
import Player (WhichPlayer (..))

initHandLength :: WhichPlayer -> Turn -> Int
initHandLength which first
  | which == first = maxHandLength
  | otherwise = maxHandLength - 1

startProgram :: Turn -> Beta.Program ()
startProgram turn = do
  replicateM_ (initHandLength PlayerA turn) (Beta.draw PlayerA PlayerA 0.25)
  replicateM_ (initHandLength PlayerB turn) (Beta.draw PlayerB PlayerB 0.25)

tutorialProgram :: Beta.Program ()
tutorialProgram = do
  Beta.raw $ do
    Alpha.setDeck PlayerA [Cards.blazeSword]
    Alpha.setDeck PlayerB []
  Beta.draw PlayerA PlayerA 0.25
