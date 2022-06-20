module Start where

import CardAnim (TimeModifier (..))
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
  replicateM_ (initHandLength PlayerA turn) (Beta.draw PlayerA PlayerA (TimeModifierOutQuint 0.25))
  replicateM_ (initHandLength PlayerB turn) (Beta.draw PlayerB PlayerB (TimeModifierOutQuint 0.25))

tutorialProgram :: Beta.Program ()
tutorialProgram = do
  Beta.raw $ do
    let makeDeck = take 25 . cycle
    let deckA = makeDeck [Cards.blazeSword, Cards.blazeGrail, Cards.blazeSword]
    Alpha.setDeck PlayerA deckA
    Alpha.setMaxLife PlayerB 15
    Alpha.setLife PlayerB 15
  Beta.null
  Beta.draw PlayerA PlayerA (TimeModifierOutQuint 0.25)

puzzle :: Beta.Program ()
puzzle = do
  Beta.raw $ do
    let deckA = take 5 $ cycle [Cards.morphSword, Cards.morphWand, Cards.morphCoin, Cards.morphGrail]
    Alpha.setDeck PlayerA deckA
    let deckB = take 25 $ cycle [Cards.mirrorSword, Cards.mirrorSword, Cards.mirrorSword, Cards.mirrorWand, Cards.mirrorCoin]
    Alpha.setDeck PlayerB deckB
    Alpha.setMaxLife PlayerA 15
    Alpha.setLife PlayerA 15
    Alpha.setMaxLife PlayerB 15
    Alpha.setLife PlayerB 15
    Alpha.setTurn PlayerB
  Beta.null
  replicateM_ 4 (Beta.draw PlayerA PlayerA (TimeModifierOutQuint 0.25))
  replicateM_ 5 (Beta.draw PlayerB PlayerB (TimeModifierOutQuint 0.25))

roundEndProgram :: Beta.Program ()
roundEndProgram = do
  Beta.raw Alpha.swapTurn
  Beta.raw Alpha.resetPasses
  Beta.draw PlayerA PlayerA (TimeModifierOutQuint 1)
  Beta.draw PlayerB PlayerB (TimeModifierOutQuint 1)

tutorialRoundEndProgram :: Beta.Program ()
tutorialRoundEndProgram = do
  Beta.raw Alpha.swapTurn
  Beta.raw Alpha.resetPasses
  Beta.draw PlayerA PlayerA (TimeModifierOutQuint 1)
