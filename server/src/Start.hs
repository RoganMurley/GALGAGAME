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
  replicateM_ (initHandLength PlayerA turn) (Beta.draw PlayerA PlayerA 0.25)
  replicateM_ (initHandLength PlayerB turn) (Beta.draw PlayerB PlayerB 0.25)


tutorialStartProgram :: Turn -> Beta.Program ()
tutorialStartProgram turn = do
  Beta.raw $ Alpha.setDeck PlayerA $ cycle [ Cards.heavenSword, Cards.blazeSword, Cards.alchemyGrail, Cards.heavenWand, Cards.mirrorSword ]
  Beta.raw $ Alpha.setDeck PlayerB $ cycle [ Cards.blazeSword, Cards.heavenSword, Cards.alchemyGrail, Cards.mirrorCoin, Cards.blazeWand ]
  startProgram turn
