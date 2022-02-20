module Start where

import qualified Cards
import Control.Monad (replicateM_)
import qualified DSL.Alpha as Alpha
import qualified DSL.Beta as Beta
import Model (Turn, maxHandLength)
import Player (WhichPlayer (..))
import qualified Stack
import StackCard (StackCard (..))

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
    let deckA = take 25 $ cycle [Cards.blazeSword, Cards.blazeSword, Cards.blazeWand]
    Alpha.setDeck PlayerA deckA
    Alpha.setMaxLife PlayerA 20
    Alpha.setLife PlayerA 20

    let deckB = take 25 $ cycle [Cards.alchemyGrail]
    Alpha.setDeck PlayerB deckB
    Alpha.setMaxLife PlayerB 20
    Alpha.setLife PlayerB 20
  Beta.null
  replicateM_ 5 (Beta.draw PlayerA PlayerA 0.25)
  replicateM_ 5 (Beta.draw PlayerB PlayerB 0.25)

-- tutorialProgram :: Beta.Program ()
-- tutorialProgram = do
--   Beta.raw $ do
--     let deckA = take 25 $ cycle [Cards.heavenSword, Cards.heavenSword, Cards.heavenWand, Cards.heavenGrail, Cards.heavenCoin]
--     Alpha.setDeck PlayerA deckA
--     let deckB = take 25 $ cycle [Cards.blazeSword, Cards.blazeSword, Cards.blazeSword, Cards.blazeGrail, Cards.blazeWand]
--     Alpha.setDeck PlayerB deckB
--   Beta.null
--   replicateM_ 5 (Beta.draw PlayerA PlayerA 0.25)
--   replicateM_ 5 (Beta.draw PlayerB PlayerB 0.25)

puzzle :: Beta.Program ()
puzzle = do
  Beta.raw $ do
    let deckA = take 5 $ cycle [Cards.morphSword, Cards.morphWand, Cards.morphCoin, Cards.morphGrail]
    Alpha.setDeck PlayerA deckA
    let deckB = take 25 $ cycle [Cards.mirrorSword, Cards.mirrorSword, Cards.mirrorSword, Cards.mirrorWand, Cards.mirrorCoin]
    Alpha.setDeck PlayerB deckB
    Alpha.setMaxLife PlayerA 15
    Alpha.setMaxLife PlayerB 15
    Alpha.setTurn PlayerB
  -- Alpha.modStack (\s -> Stack.set s 1 (Just (StackCard PlayerB Cards.blazeSword)))
  Beta.null
  replicateM_ 4 (Beta.draw PlayerA PlayerA 0.25)
  replicateM_ 5 (Beta.draw PlayerB PlayerB 0.25)
