module Start where

import CardAnim (CardAnim (..), TimeModifier (..))
import qualified Cards
import Control.Monad (replicateM_)
import qualified DSL.Alpha as Alpha
import qualified DSL.Beta as Beta
import Data.Text (Text)
import Model (Turn, maxHandLength)
import Player (WhichPlayer (..))
import StackCard (StackCard (..))
import Wheel (Wheel (..))

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
    Alpha.setMaxLife PlayerB 20
    Alpha.setLife PlayerB 20
    Alpha.modStack
      ( \s ->
          s
            { wheel_3 =
                Just $
                  StackCard
                    { stackcard_card = Cards.blazeSword,
                      stackcard_owner = PlayerA
                    },
              wheel_6 =
                Just $
                  StackCard
                    { stackcard_card = Cards.heavenSword,
                      stackcard_owner = PlayerA
                    },
              wheel_9 =
                Just $
                  StackCard
                    { stackcard_card = Cards.mirrorSword,
                      stackcard_owner = PlayerA
                    }
            }
      )
  Beta.rawAnim $ Announce "TUTORIAL" (TimeModifierLinear 3)

tutorial1Program :: Maybe (Text, Text) -> Beta.Program ()
tutorial1Program _ = do
  Beta.raw $ do
    let makeDeck = take 25 . cycle
    let deckA = makeDeck [Cards.blazeSword, Cards.heavenSword, Cards.mirrorSword]
    Alpha.setDeck PlayerA deckA
    Alpha.setMaxLife PlayerB 20
    Alpha.setLife PlayerB 20

tutorial2Program :: Maybe (Text, Text) -> Beta.Program ()
tutorial2Program _ = do
  Beta.raw $ do
    let makeDeck = take 25 . cycle
    let deckA = makeDeck [Cards.blazeSword, Cards.blazeGrail, Cards.blazeWand]
    Alpha.setDeck PlayerA deckA
    Alpha.setMaxLife PlayerB 50
    Alpha.setLife PlayerB 50
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

passiveRoundEndProgram :: Beta.Program ()
passiveRoundEndProgram = do
  Beta.raw Alpha.swapTurn
  Beta.raw Alpha.resetPasses
  Beta.draw PlayerA PlayerA (TimeModifierOutQuint 1)

noDrawRoundEndProgram :: Beta.Program ()
noDrawRoundEndProgram = do
  Beta.raw Alpha.swapTurn
  Beta.raw Alpha.resetPasses
