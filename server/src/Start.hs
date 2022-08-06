module Start where

import CardAnim (CardAnim (..), TimeModifier (..))
import qualified Cards
import Control.Monad (replicateM_)
import qualified DSL.Alpha as Alpha
import qualified DSL.Beta as Beta
import Data.Text (Text)
import DeckBuilding (Character (..), blazeRune, characterCards, heavenRune, mirrorRune, shroomRune, tideRune)
import Model (Turn, maxHandLength, setForceWin)
import Player (WhichPlayer (..), other)
import Util (shuffle, split)

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
    let deckA = makeDeck [Cards.blazeSword, Cards.blazeGrail, Cards.blazeWand, Cards.blazeSword]
    Alpha.setDeck PlayerA deckA
    Alpha.setMaxLife PlayerA 20
    Alpha.setLife PlayerA 20
    let deckB = makeDeck [Cards.heavenSword, Cards.heavenGrail, Cards.heavenSword, Cards.heavenSword, Cards.heavenWand]
    Alpha.setDeck PlayerB deckB
    Alpha.setMaxLife PlayerB 20
    Alpha.setLife PlayerB 20

tutorial1Program :: Maybe (Text, Text) -> Beta.Program ()
tutorial1Program _ = do
  (ga, gb) <- split <$> Beta.getGen
  Beta.raw $ do
    let deckA = shuffle ga $ characterCards (Character (Left (blazeRune, heavenRune, shroomRune)) 50)
    Alpha.setDeck PlayerA deckA
    let deckB = shuffle gb $ characterCards (Character (Left (heavenRune, mirrorRune, tideRune)) 50)
    Alpha.setDeck PlayerB deckB
  replicateM_ 6 (Beta.draw PlayerA PlayerA (TimeModifierOutQuint 0.25))
  replicateM_ 5 (Beta.draw PlayerB PlayerB (TimeModifierOutQuint 0.25))

-- tutorial2Program :: Maybe (Text, Text) -> Beta.Program ()
-- tutorial2Program _ = do
--   Beta.raw $ do
--     let makeDeck = take 25 . cycle
--     let deckA = makeDeck [Cards.tideCoin, Cards.heavenSword, Cards.tideSword, Cards.heavenGrail, Cards.heavenWand, Cards.tideWand, Cards.heavenCoin, Cards.heavenSword]
--     Alpha.setDeck PlayerA deckA
--     Alpha.setMaxLife PlayerA 50
--     Alpha.setLife PlayerA 50
--     let deckB = makeDeck [Cards.mirrorSword, Cards.alchemySword, Cards.mirrorWand, Cards.alchemyWand, Cards.mirrorCoin, Cards.alchemyGrail]
--     Alpha.setDeck PlayerB deckB
--     Alpha.setMaxLife PlayerB 50
--     Alpha.setLife PlayerB 50
--   replicateM_ 5 (Beta.draw PlayerA PlayerA (TimeModifierOutQuint 0.25))
--   replicateM_ 6 (Beta.draw PlayerB PlayerB (TimeModifierOutQuint 0.25))

-- tutorial3Program :: Maybe (Text, Text) -> Beta.Program ()
-- tutorial3Program _ = do
--   Beta.raw $ do
--     genA <- Alpha.getGen
--     let makeDeck = \g cs -> shuffle g $ take 25 . cycle $ cs
--     let deckA =
--           makeDeck
--             genA
--             [ Cards.blazeSword,
--               Cards.blazeGrail,
--               Cards.blazeWand,
--               Cards.blazeCoin,
--               Cards.tideSword,
--               Cards.tideWand,
--               Cards.tideGrail,
--               Cards.tideCoin
--             ]
--     Alpha.setDeck PlayerA deckA
--     Alpha.setMaxLife PlayerA 20
--     Alpha.setLife PlayerA 20
--     let (genB, _) = split genA
--     let deckB =
--           makeDeck
--             genB
--             [ Cards.blazeSword,
--               Cards.blazeGrail,
--               Cards.blazeWand,
--               Cards.blazeCoin,
--               Cards.tideSword,
--               Cards.tideWand,
--               Cards.tideGrail,
--               Cards.tideCoin
--             ]
--     Alpha.setDeck PlayerB deckB
--     Alpha.setMaxLife PlayerB 20
--     Alpha.setLife PlayerB 20
--   replicateM_ 5 (Beta.draw PlayerA PlayerA (TimeModifierOutQuint 0.25))
--   replicateM_ 6 (Beta.draw PlayerB PlayerB (TimeModifierOutQuint 0.25))

puzzle :: Maybe (Text, Text) -> Beta.Program ()
puzzle _ = do
  Beta.raw $ do
    let deckA = take 5 $ cycle [Cards.morphSword, Cards.morphWand, Cards.morphCoin, Cards.morphGrail]
    let deckB = []
    Alpha.setDeck PlayerA deckA
    Alpha.setDeck PlayerB deckB
    Alpha.setMaxLife PlayerA 25
    Alpha.setLife PlayerA 25
    Alpha.setMaxLife PlayerB 25
    Alpha.setLife PlayerB 25
    Alpha.setTurn PlayerB
  Beta.rawAnim $ Announce "MORPH PUZZLE" (TimeModifierLinear 3)
  Beta.rawAnim $ Announce "WIN THIS\nROUND" (TimeModifierLinear 3)
  replicateM_ 4 (Beta.draw PlayerA PlayerA (TimeModifierOutQuint 1))

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

defeatRoundEndProgram :: WhichPlayer -> Beta.Program ()
defeatRoundEndProgram w = do
  Beta.raw $ Alpha.modMisc (setForceWin (other w))
  Beta.rawAnim $ GameEnd (Just $ other w)
