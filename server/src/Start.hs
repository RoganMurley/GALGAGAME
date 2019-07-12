module Start where

import Control.Monad (replicateM_)
import Model (Turn, maxHandLength)
import Player (WhichPlayer(..))

import qualified Cards
import qualified DSL.Alpha as Alpha
import qualified DSL.Beta as Beta


initHandLength :: WhichPlayer -> Turn -> Int
initHandLength which first
  | which == first = maxHandLength
  | otherwise      = maxHandLength - 1


startProgram :: Turn -> Beta.Program ()
startProgram turn = do
  replicateM_ (initHandLength PlayerA turn) (Beta.draw PlayerA PlayerA)
  replicateM_ (initHandLength PlayerB turn) (Beta.draw PlayerB PlayerB)


tutorialStartProgram :: Turn -> Beta.Program ()
tutorialStartProgram turn = do
  Beta.raw $ Alpha.setDeck PlayerA $ cycle [ Cards.hammer, Cards.missile, Cards.potion, Cards.lightning, Cards.grudge ]
  Beta.raw $ Alpha.setDeck PlayerB $ cycle [ Cards.missile, Cards.hammer, Cards.potion, Cards.reflect, Cards.fireball ]
  startProgram turn
