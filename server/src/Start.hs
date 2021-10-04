module Start where

import Control.Monad (replicateM_)
import Model (Turn, maxHandLength)
import Player (WhichPlayer(..))

import qualified DSL.Beta as Beta


initHandLength :: WhichPlayer -> Turn -> Int
initHandLength which first
  | which == first = maxHandLength
  | otherwise      = maxHandLength - 1


startProgram :: Turn -> Beta.Program ()
startProgram turn = do
  replicateM_ (initHandLength PlayerA turn) (Beta.draw PlayerA PlayerA 0.25)
  replicateM_ (initHandLength PlayerB turn) (Beta.draw PlayerB PlayerB 0.25)
