{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module DSL.Beta.Interpreters where

import Bounce (CardBounce (..))
import Card (Card)
import CardAnim (Damage, Hurt, TimeModifier (..), cardAnimDamage)
import CardAnim qualified
import {-# SOURCE #-} Cards (getEndCard)
import Control.Monad (when)
import Control.Monad.Freer (Eff, Member, reinterpret, run, send)
import Control.Monad.Freer.State as S
import DSL.Alpha qualified as Alpha
import DSL.Anim qualified as Anim
import DSL.Beta.DSL
import Data.Maybe (fromMaybe, isJust)
import Discard (CardDiscard (..), isDiscard)
import HandCard (HandCard (..), anyCard, isRevealed, knownCard)
import Life (Life)
import Model (Model, gameover, getNoDraws, maxHandLength)
import ModelDiff (ModelDiff)
import ModelDiff qualified
import Player (WhichPlayer (..))
import ResolveData (ResolveData (..))
import Safe (headMay)
import Transmutation (Transmutation (..))
import Util (xor)
import Wheel (Wheel (..))
import Wheel qualified

alphaI :: DSL a -> Alpha.Program a
alphaI (Raw p) = p
alphaI (Hurt d w _) = Alpha.hurt d w
alphaI (PayLife d w) = Alpha.hurt d w
alphaI (Heal h w) = Alpha.heal h w
alphaI (Draw' w d _) = Alpha.draw w d
alphaI (AddToHand w c) = Alpha.addToHand w c
alphaI (Play w c i) = Alpha.play w (anyCard c) i
alphaI (Transmute' t) = Alpha.transmute t
alphaI (TransmuteActive' t) = Alpha.transmuteActive t
alphaI Rotate = Alpha.rotate
alphaI Windup = Alpha.windup
alphaI (Bounce' b _) = Alpha.bounce b
alphaI (BounceDeck' b _) = Alpha.bounceDeck b
alphaI (DiscardStack' f) = Alpha.discardStack f
alphaI (DiscardHand w f) = Alpha.discardHand w f
alphaI (MoveStack' f _) = Alpha.moveStack f
alphaI (Mill w _) = Alpha.mill w
alphaI (Reveal w f) = Alpha.reveal w f
alphaI (RevealDeck w) = Alpha.revealDeck w
alphaI GetGen = Alpha.getGen
alphaI GetRot = Alpha.getRot
alphaI (GetLife w) = Alpha.getLife w
alphaI (GetHand w) = Alpha.getHand w
alphaI (GetDeck w) = Alpha.getDeck w
alphaI GetStack = Alpha.getStack
alphaI GetHold = Alpha.getHold
alphaI GetModel = Alpha.getModel
alphaI (RawAnim _) = return ()
alphaI UnknownDamage = return ()
alphaI Null = return ()

data ExecDSL n where
  ExecAlpha :: Alpha.DSL a -> ExecDSL a
  ExecAnim :: Anim.DSL a -> ExecDSL a

type ExecProgram a = Eff '[ExecDSL] a

basicAnim :: Anim.DSL () -> Eff '[ExecDSL] a -> Eff '[ExecDSL] a
basicAnim anim alpha = alpha <* (send . ExecAnim $ anim)

animI :: DSL a -> Eff '[ExecDSL] a -> Eff '[ExecDSL] a
animI Null = basicAnim Anim.Null
animI Rotate = basicAnim Anim.Rotate
animI Windup = basicAnim Anim.Windup
animI (RawAnim r) = basicAnim $ Anim.Raw r
animI GetGen = basicAnim Anim.GetGen
animI UnknownDamage = basicAnim Anim.UnknownDamage
animI (Hurt d w h) = damageAnim d w h
animI (PayLife d w) = damageAnim d w CardAnim.Curse
animI (Heal _ w) = healAnim w
animI (AddToHand w c) = addToHandAnim w c
animI (Draw' w d t) = drawAnim w d t
animI (Play w c i) = playAnim w c i
animI (Transmute' t) = transmuteAnim t
animI (TransmuteActive' t) = transmuteActiveAnim t
animI (Bounce' b t) = bounceAnim b t
animI (BounceDeck' b t) = bounceDeckAnim b t
animI (DiscardStack' d) = discardStackAnim d
animI (DiscardHand w f) = discardHandAnim w f
animI (MoveStack' m t) = moveStackAnim m t
animI (Mill w t) = millAnim w t
animI (Reveal w f) = revealAnim w f
animI (RevealDeck w) = revealDeckAnim w
animI _ = id

execAnim :: Anim.Program a -> Eff '[ExecDSL] a
execAnim = reinterpret (send . ExecAnim)

execAlpha :: Alpha.Program a -> Eff '[ExecDSL] a
execAlpha = reinterpret (send . ExecAlpha)

damageAnim :: Life -> WhichPlayer -> Hurt -> Eff '[ExecDSL] a -> Eff '[ExecDSL] a
damageAnim d w h alpha = do
  final <- alpha
  when (d > 0) $ execAnim $ Anim.hurt w d h
  return final

healAnim :: WhichPlayer -> Eff '[ExecDSL] a -> Eff '[ExecDSL] a
healAnim w alpha = do
  oldLife <- execAlpha $ Alpha.getLife w
  final <- alpha
  newLife <- execAlpha $ Alpha.getLife w
  let lifeChange = newLife - oldLife
  when (lifeChange > 0) $ execAnim $ Anim.heal w lifeChange
  return final

drawAnim :: WhichPlayer -> WhichPlayer -> TimeModifier -> Eff '[ExecDSL] a -> Eff '[ExecDSL] a
drawAnim w d t alpha = do
  nextCard <- execAlpha $ headMay <$> Alpha.getDeck d
  handLength <- execAlpha $ length <$> Alpha.getHand w
  noDraws <- execAlpha $ getNoDraws w <$> Alpha.getMisc
  final <- alpha
  if handLength < maxHandLength
    then execAnim $ Anim.draw w t
    else execAnim $ Anim.mill w (fromMaybe (getEndCard noDraws) (anyCard <$> nextCard)) (TimeModifierOutQuint 1)
  return final

millAnim :: WhichPlayer -> TimeModifier -> Eff '[ExecDSL] a -> Eff '[ExecDSL] a
millAnim w t alpha = do
  nextCard <- execAlpha $ headMay <$> Alpha.getDeck w
  noDraws <- execAlpha $ getNoDraws w <$> Alpha.getMisc
  final <- alpha
  execAnim $ Anim.mill w (fromMaybe (getEndCard noDraws) (anyCard <$> nextCard)) t
  return final

addToHandAnim :: WhichPlayer -> HandCard -> Eff '[ExecDSL] a -> Eff '[ExecDSL] a
addToHandAnim w c alpha = do
  handLength <- execAlpha $ length <$> Alpha.getHand w
  final <- alpha
  if handLength < maxHandLength
    then execAnim $ Anim.draw w (TimeModifierOutQuint 1)
    else execAnim $ Anim.mill w (anyCard c) (TimeModifierOutQuint 1)
  return final

playAnim :: WhichPlayer -> HandCard -> Int -> Eff '[ExecDSL] a -> Eff '[ExecDSL] a
playAnim w c i alpha = do
  final <- alpha
  execAnim $ Anim.play w c i
  return final

transmuteAnim :: Wheel (Maybe Transmutation) -> Eff '[ExecDSL] a -> Eff '[ExecDSL] a
transmuteAnim transmutations alpha = do
  let activity = any isJust transmutations
  when activity (execAnim $ Anim.transmute transmutations)
  final <- alpha
  when activity (execAnim Anim.null)
  return final

transmuteActiveAnim :: Transmutation -> Eff '[ExecDSL] a -> Eff '[ExecDSL] a
transmuteActiveAnim transmutation alpha = do
  let transmutations = Wheel.init (\i -> if i == 0 then Just transmutation else Nothing)
  final <- alpha
  execAnim $ Anim.transmute transmutations
  return final

bounceAnim :: Wheel (Maybe CardBounce) -> TimeModifier -> Eff '[ExecDSL] a -> Eff '[ExecDSL] a
bounceAnim bounces t alpha = do
  let activity = any isJust bounces
  when activity (execAnim $ Anim.bounce bounces t)
  final <- alpha
  when activity (execAnim Anim.null)
  return final

bounceDeckAnim :: Wheel Bool -> TimeModifier -> Eff '[ExecDSL] a -> Eff '[ExecDSL] a
bounceDeckAnim bounces t alpha = do
  let activity = any id bounces
  when activity (execAnim $ Anim.bounceDeck bounces t)
  final <- alpha
  when activity (execAnim Anim.null)
  return final

moveStackAnim :: Wheel (Maybe Int) -> TimeModifier -> Eff '[ExecDSL] a -> Eff '[ExecDSL] a
moveStackAnim moves t alpha = do
  let activity = any isJust moves
  when activity (execAnim $ Anim.moveStack moves t)
  final <- alpha
  when activity (execAnim Anim.null)
  return final

discardStackAnim :: Wheel Bool -> Eff '[ExecDSL] a -> Eff '[ExecDSL] a
discardStackAnim discards alpha = do
  let activity = any id discards
  when activity (execAnim $ Anim.discardStack discards)
  final <- alpha
  when activity (execAnim Anim.null)
  return final

discardHandAnim :: WhichPlayer -> (Int -> Card -> Bool) -> Eff '[ExecDSL] a -> Eff '[ExecDSL] a
discardHandAnim w f alpha = do
  discards <- getHandDiscards w f
  let activity = any isDiscard discards
  final <- alpha
  when activity (execAnim $ Anim.discardHand w discards)
  return final

getHandDiscards :: WhichPlayer -> (Int -> Card -> Bool) -> Eff '[ExecDSL] [CardDiscard]
getHandDiscards w f = do
  hand <- execAlpha $ Alpha.getHand w
  return $ getDiscards' 0 0 hand
  where
    getDiscards' :: Int -> Int -> [HandCard] -> [CardDiscard]
    getDiscards' handIndex finalHandIndex (card : rest) =
      if f handIndex $ anyCard card
        then CardDiscard card : getDiscards' (handIndex + 1) finalHandIndex rest
        else NoDiscard (knownCard card) finalHandIndex : getDiscards' (handIndex + 1) (finalHandIndex + 1) rest
    getDiscards' _ _ [] = []

revealAnim :: WhichPlayer -> (Int -> Card -> Bool) -> Eff '[ExecDSL] a -> Eff '[ExecDSL] a
revealAnim w f alpha = do
  hand <- execAlpha $ Alpha.getHand w
  let initialReveals = isRevealed <$> hand :: [Bool]
  let reveals = uncurry f <$> zip [0 ..] (anyCard <$> hand) :: [Bool]
  final <- alpha
  let activity = any id $ uncurry xor <$> zip initialReveals reveals :: Bool
  when activity (execAnim $ Anim.reveal w reveals)
  return final

revealDeckAnim :: WhichPlayer -> Eff '[ExecDSL] a -> Eff '[ExecDSL] a
revealDeckAnim w alpha = do
  initialDeck <- execAlpha $ Alpha.getDeck w
  let initialRevealed = filter isRevealed initialDeck
  final <- alpha
  finalDeck <- execAlpha $ Alpha.getDeck w
  let finalRevealed = filter isRevealed finalDeck
  let newlyRevealed = drop (length initialRevealed) finalRevealed
  mapM_ (\c -> execAnim $ Anim.revealDeck w (anyCard c)) newlyRevealed
  return final

betaI :: forall a. Program a -> Eff '[ExecDSL] a
betaI = reinterpret (\x -> animI x $ execAlpha $ alphaI x)

execute :: forall a. Model -> Eff '[ExecDSL] a -> (Model, [ResolveData])
execute initialModel prog =
  (\(finalModel, _, finalResReversed, _) -> (finalModel, reverse finalResReversed)) result
  where
    initialState :: (Model, ModelDiff, [ResolveData], Bool)
    initialState = (initialModel, mempty, [], False)
    part_a :: Eff '[S.State (Model, ModelDiff, [ResolveData], Bool)] a
    part_a = reinterpret go prog
    part_b :: Eff '[] (Model, ModelDiff, [ResolveData], Bool)
    part_b = execState initialState part_a
    result :: (Model, ModelDiff, [ResolveData], Bool)
    result = run part_b
    go :: (Member (S.State (Model, ModelDiff, [ResolveData], Bool)) effs) => forall b. ExecDSL b -> Eff effs b
    go (ExecAlpha alpha) = do
      (model, diff, res :: [ResolveData], terminated) <- S.get
      let (newDiff, next) = Alpha.alphaEffI model alpha
      let newModel = ModelDiff.update model newDiff
      when (not terminated) $ S.put (newModel, diff <> newDiff, res, terminated)
      return next
    go (ExecAnim anim) = do
      (model :: Model, diff :: ModelDiff, res :: [ResolveData], terminated :: Bool) <- S.get
      let cardAnim = Anim.animate anim
      let damage = fromMaybe (0, 0) $ cardAnimDamage <$> cardAnim
      let newRes = ResolveData diff cardAnim damage : res
      when (not terminated) (S.put (model :: Model, mempty :: ModelDiff, newRes :: [ResolveData], terminated || gameover model))
      return $ Anim.next anim

damageNumbersI :: Model -> Program () -> (Damage, Damage)
damageNumbersI model program =
  let (_, resolveData) = execute model $ betaI program
      damage = resolveData_animDamage <$> resolveData :: [(Damage, Damage)]
      damagePa = sum $ fst <$> damage :: Damage
      damagePb = sum $ snd <$> damage :: Damage
   in (damagePa, damagePb)
