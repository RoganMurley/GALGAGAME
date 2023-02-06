module Start where

import Card (Card (..))
import CardAnim (CardAnim (..), TimeModifier (..))
import qualified Cards
import Control.Monad (replicateM_)
import qualified DSL.Alpha as Alpha
import qualified DSL.Beta as Beta
import Data.Text (Text)
import DeckBuilding (Character (..), angelRune, characterCards, fireRune, goldRune, mirrorRune, shroomRune, waterRune)
import HandCard (HandCard (..), anyCard)
import Model (Passes (OnePass), maxHandLength, setForceWin)
import Player (WhichPlayer (..), other)
import Util (shuffle, split)
import Prelude hiding (init)

initDeck :: WhichPlayer -> Beta.Program ()
initDeck w =
  Beta.raw $ do
    deck <- Alpha.getDeck w
    newDeck <-
      mapM
        ( \handCard ->
            let card = anyCard handCard
                init = card_init card
             in do
                  newCard <- init card w
                  gen <- Alpha.getGen
                  let (newGen, _) = split gen
                  Alpha.setGen newGen
                  return newCard
        )
        deck
    Alpha.setDeck w (HandCard <$> newDeck)

initProgram :: Beta.Program ()
initProgram = do
  initDeck PlayerA
  initDeck PlayerB

startProgram :: Maybe (Text, Text) -> Beta.Program ()
startProgram mUsernames = do
  case mUsernames of
    Just (usernamePa, usernamePb) -> do
      let announcement = usernamePa <> " vs " <> usernamePb
      Beta.rawAnim $ Announce announcement (TimeModifierLinear 3)
    Nothing ->
      return ()
  replicateM_ maxHandLength (Beta.draw PlayerA PlayerA (TimeModifierOutQuint 0.25))
  replicateM_ maxHandLength (Beta.draw PlayerB PlayerB (TimeModifierOutQuint 0.25))

tutorial0Program :: Maybe (Text, Text) -> Beta.Program ()
tutorial0Program _ = do
  Beta.raw $ do
    let makeDeck = take 25 . cycle . fmap HandCard
    let deckA = makeDeck [Cards.fireSword, Cards.fireCup, Cards.fireWand, Cards.fireSword]
    Alpha.setPasses OnePass
    Alpha.setDeck PlayerA deckA
    Alpha.setMaxLife PlayerA 20
    Alpha.setLife PlayerA 20
    let deckB = makeDeck [Cards.angelSword, Cards.angelCup, Cards.angelSword, Cards.angelSword, Cards.angelWand]
    Alpha.setDeck PlayerB deckB
    Alpha.setMaxLife PlayerB 20
    Alpha.setLife PlayerB 20

tutorial1Program :: Maybe (Text, Text) -> Beta.Program ()
tutorial1Program _ = do
  (ga, gb) <- split <$> Beta.getGen
  Beta.raw $ do
    let deckA = shuffle ga $ characterCards (Character (Left (fireRune, angelRune, shroomRune)) 50)
    Alpha.setDeck PlayerA deckA
    let deckB = shuffle gb $ characterCards (Character (Left (goldRune, mirrorRune, waterRune)) 50)
    Alpha.setDeck PlayerB deckB
  replicateM_ 6 (Beta.draw PlayerA PlayerA (TimeModifierOutQuint 0.25))
  replicateM_ 5 (Beta.draw PlayerB PlayerB (TimeModifierOutQuint 0.25))

-- tutorial2Program :: Maybe (Text, Text) -> Beta.Program ()
-- tutorial2Program _ = do
--   Beta.raw $ do
--     let makeDeck = take 25 . cycle
--     let deckA = makeDeck [Cards.waterCoin, Cards.angelSword, Cards.waterSword, Cards.angelCup, Cards.angelWand, Cards.waterWand, Cards.angelCoin, Cards.angelSword]
--     Alpha.setDeck PlayerA deckA
--     Alpha.setMaxLife PlayerA 50
--     Alpha.setLife PlayerA 50
--     let deckB = makeDeck [Cards.mirrorSword, Cards.alchemySword, Cards.mirrorWand, Cards.alchemyWand, Cards.mirrorCoin, Cards.alchemyCup]
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
--             [ Cards.fireSword,
--               Cards.fireCup,
--               Cards.fireWand,
--               Cards.fireCoin,
--               Cards.waterSword,
--               Cards.waterWand,
--               Cards.waterCup,
--               Cards.waterCoin
--             ]
--     Alpha.setDeck PlayerA deckA
--     Alpha.setMaxLife PlayerA 20
--     Alpha.setLife PlayerA 20
--     let (genB, _) = split genA
--     let deckB =
--           makeDeck
--             genB
--             [ Cards.fireSword,
--               Cards.fireCup,
--               Cards.fireWand,
--               Cards.fireCoin,
--               Cards.waterSword,
--               Cards.waterWand,
--               Cards.waterCup,
--               Cards.waterCoin
--             ]
--     Alpha.setDeck PlayerB deckB
--     Alpha.setMaxLife PlayerB 20
--     Alpha.setLife PlayerB 20
--   replicateM_ 5 (Beta.draw PlayerA PlayerA (TimeModifierOutQuint 0.25))
--   replicateM_ 6 (Beta.draw PlayerB PlayerB (TimeModifierOutQuint 0.25))

puzzle :: Maybe (Text, Text) -> Beta.Program ()
puzzle _ = do
  Beta.raw $ do
    let deckA = take 5 . cycle $ HandCard <$> [Cards.claySword, Cards.clayWand, Cards.clayCoin, Cards.clayCup]
    let deckB = []
    Alpha.setDeck PlayerA deckA
    Alpha.setDeck PlayerB deckB
    Alpha.setMaxLife PlayerA 25
    Alpha.setLife PlayerA 25
    Alpha.setMaxLife PlayerB 25
    Alpha.setLife PlayerB 25
    Alpha.setTurn PlayerB
  Beta.rawAnim $ Announce "CLAY PUZZLE" (TimeModifierLinear 3)
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
