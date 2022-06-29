module Start where

import CardAnim (CardAnim (..), TimeModifier (..))
import qualified Cards
import Control.Monad (replicateM_)
import qualified DSL.Alpha as Alpha
import qualified DSL.Beta as Beta
import Data.Text (Text)
import Model (Turn, maxHandLength)
import Player (WhichPlayer (..))

initHandLength :: WhichPlayer -> Turn -> Int
initHandLength which first
  | which == first = maxHandLength
  | otherwise = maxHandLength - 1

startProgram :: Turn -> Maybe (Text, Text) -> Beta.Program ()
startProgram turn mUsernames = do
  case mUsernames of
    Just (usernamePa, usernamePb) -> do
      let announcement = usernamePa <> " vs " <> usernamePb
      Beta.rawAnim $ Announce announcement (TimeModifierLinear 3)
    Nothing ->
      return ()
  replicateM_ (initHandLength PlayerA turn) (Beta.draw PlayerA PlayerA (TimeModifierOutQuint 0.25))
  replicateM_ (initHandLength PlayerB turn) (Beta.draw PlayerB PlayerB (TimeModifierOutQuint 0.25))

tutorial0Program :: Maybe (Text, Text) -> Beta.Program ()
tutorial0Program _ = do
  Beta.raw $ do
    let makeDeck = take 25 . cycle
    let deckA = makeDeck [Cards.blazeSword, Cards.blazeGrail, Cards.blazeSword]
    Alpha.setDeck PlayerA deckA
    Alpha.setMaxLife PlayerA 15
    Alpha.setLife PlayerA 15
    Alpha.setMaxLife PlayerB 15
    Alpha.setLife PlayerB 15
  -- Beta.rawAnim $ Announce "BEHOLD THE\nWHEEL OF FATE" (TimeModifierLinear 3)
  -- Beta.rawAnim $ Announce "DO NOT BE\nAFRAID!" (TimeModifierLinear 3)
  -- Beta.rawAnim $ Announce "I AM GALGA,\nFATEFUL ANGEL" (TimeModifierLinear 3)
  -- Beta.rawAnim $ Announce "NOW TURN\nME..." (TimeModifierLinear 3)
  -- Beta.rawAnim $ Announce "...AND SEIZE\nYOUR DESTINY!" (TimeModifierLinear 3)
  -- Beta.rawAnim $ Announce "I AM THE ANGEL\nGALGA" (TimeModifierLinear 3)
  -- Beta.rawAnim $ Announce "I AM AN\nANGEL..." (TimeModifierLinear 3)
  -- Beta.rawAnim $ Announce "...AND I AM A\nMACHINE" (TimeModifierLinear 3)
  -- Beta.rawAnim $ Announce "TOGETHER, WE\nCAN ESCAPE..." (TimeModifierLinear 3)
  -- Beta.rawAnim $ Announce "TOGETHER, WE\nCAN ESCAPE..." (TimeModifierLinear 3)
  -- Beta.rawAnim $ Announce "DO NOT BE\nAFRAID" (TimeModifierLinear 3)
  -- Beta.rawAnim $ Announce "I AM AN\nANGEL..." (TimeModifierLinear 3)
  -- Beta.rawAnim $ Announce "...AND I AM A\nMACHINE" (TimeModifierLinear 3)
  -- Beta.rawAnim $ Announce "WHEN MY WHEEL\nTURNS...\n" (TimeModifierLinear 3)
  -- Beta.rawAnim $ Announce "...THE WORLD\nFOLLOWS" (TimeModifierLinear 3)
  -- replicateM_ 8 Beta.windup
  Beta.draw PlayerA PlayerA (TimeModifierOutQuint 1)

tutorial1Program :: Maybe (Text, Text) -> Beta.Program ()
tutorial1Program _ = do
  Beta.raw $ do
    let makeDeck = take 25 . cycle
    let deckA = makeDeck [Cards.blazeSword, Cards.blazeGrail, Cards.blazeWand]
    Alpha.setDeck PlayerA deckA
    Alpha.setMaxLife PlayerB 50
    Alpha.setLife PlayerB 50
  -- Beta.rawAnim $ Announce "MY BODY IS THE\nWHEEL..." (TimeModifierLinear 3)
  -- Beta.rawAnim $ Announce "...THAT TURNS THE\nHEAVENS!" (TimeModifierLinear 3)
  replicateM_ 5 (Beta.draw PlayerA PlayerA (TimeModifierOutQuint 0.25))

puzzle :: Maybe (Text, Text) -> Beta.Program ()
puzzle _ = do
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
  Beta.rawAnim $ Announce "MORPH PUZZLE" (TimeModifierLinear 3)
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
