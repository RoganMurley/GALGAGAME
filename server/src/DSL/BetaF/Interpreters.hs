{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module DSL.BetaF.Interpreters where

import Bounce (CardBounce (..))
import Card (Card)
import CardAnim (Damage, Hurt, TimeModifier (..), cardAnimDamage)
import {-# SOURCE #-} Cards (strangeEnd)
import Control.Monad (when)
import Control.Monad.Freer (Eff, Member, Members, interpose, interpret, raise, reinterpret, interpretWith, reinterpret2, run, send, subsume, translate)
import Control.Monad.Freer.State as S
import qualified DSL.AlphaF as Alpha
import qualified DSL.AnimF as Anim
import DSL.BetaF.DSL
import Data.Maybe (fromMaybe, isJust)
import Discard (CardDiscard (..), isDiscard)
import HandCard (HandCard (..), anyCard, isRevealed, knownCard)
import Life (Life)
import Model (Model, gameover, maxHandLength)
import ModelDiff (ModelDiff)
import qualified ModelDiff
import Player (WhichPlayer (..))
import ResolveData (ResolveData (..))
import Safe (headMay)
import Transmutation (Transmutation (..))
import Util (xor)
import Wheel (Wheel (..))
import qualified Wheel

alphaI :: DSL a -> Alpha.Program a
alphaI (Raw p) = p
alphaI (Hurt d w _) = Alpha.hurt d w
alphaI (Heal h w) = Alpha.heal h w
alphaI (Draw w d _) = Alpha.draw w d
alphaI (AddToHand w c) = Alpha.addToHand w c
alphaI (Play w c i) = Alpha.play w (anyCard c) i
alphaI (Transmute' t) = Alpha.transmute t
alphaI (TransmuteActive' t) = Alpha.transmuteActive t
alphaI Rotate = Alpha.rotate
alphaI Windup = Alpha.windup
alphaI (Bounce' f _) = Alpha.bounce f
alphaI (DiscardStack' f) = Alpha.discardStack f
alphaI (DiscardHand w f) = Alpha.discardHand w f
alphaI (MoveStack' f _) = Alpha.moveStack f
alphaI (Mill w _) = Alpha.mill w
alphaI (Reveal w f) = Alpha.reveal w f
alphaI GetGen = Alpha.getGen
alphaI GetRot = Alpha.getRot
alphaI (GetLife w) = Alpha.getLife w
alphaI (GetHand w) = Alpha.getHand w
alphaI (GetDeck w) = Alpha.getDeck w
alphaI GetStack = Alpha.getStack
alphaI GetHold = Alpha.getHold
alphaI GetModel = Alpha.getModel
alphaI (RawAnim _) = return ()
alphaI Null = return ()

type AlphaAnimProgram a = Eff '[Alpha.DSL, Anim.DSL] a

basicAnim :: Anim.DSL () -> AlphaAnimProgram a -> AlphaAnimProgram a
basicAnim anim alpha = alpha <* send anim

animI :: DSL a -> (AlphaAnimProgram a -> AlphaAnimProgram a)
animI Null = basicAnim Anim.Null
animI Rotate = basicAnim Anim.Rotate
animI Windup = basicAnim Anim.Windup
animI (RawAnim r) = basicAnim $ Anim.Raw r
animI GetGen = basicAnim Anim.GetGen
animI (Hurt d w h) = damageAnim d w h
animI (Heal _ w) = healAnim w
animI (AddToHand w c) = addToHandAnim w c
animI (Draw w d t) = drawAnim w d t
animI (Play w c i) = playAnim w c i
animI (Transmute' t) = transmuteAnim t
animI (TransmuteActive' t) = transmuteActiveAnim t
animI (Bounce' b t) = bounceAnim b t
animI (DiscardStack' d) = discardStackAnim d
animI (DiscardHand w f) = discardHandAnim w f
animI (MoveStack' m t) = moveStackAnim m t
animI (Mill w t) = millAnim w t
animI (Reveal w f) = revealAnim w f
animI _ = id

raiseAnim :: Anim.Program a -> AlphaAnimProgram a
raiseAnim = reinterpret2 send

raiseAlpha :: Alpha.Program a -> AlphaAnimProgram a
raiseAlpha = reinterpret2 send

damageAnim :: Life -> WhichPlayer -> Hurt -> AlphaAnimProgram a -> AlphaAnimProgram a
damageAnim d w h alpha = do
  final <- alpha
  when (d > 0) $ Anim.hurt w d h
  return final

healAnim :: WhichPlayer -> AlphaAnimProgram a -> AlphaAnimProgram a
healAnim w alpha = do
  oldLife <- Alpha.getLife w
  final <- alpha
  newLife <- Alpha.getLife w
  let lifeChange = newLife - oldLife
  when (lifeChange > 0) $ Anim.heal w lifeChange
  return final

drawAnim :: WhichPlayer -> WhichPlayer -> TimeModifier -> AlphaAnimProgram a -> AlphaAnimProgram a
drawAnim w d t alpha = do
  nextCard <- headMay <$> Alpha.getDeck d
  handLength <- length <$> Alpha.getHand w
  final <- alpha
  if handLength < maxHandLength
    then Anim.draw w t
    else Anim.mill w (fromMaybe strangeEnd nextCard) (TimeModifierOutQuint 1)
  return final

millAnim :: WhichPlayer -> TimeModifier -> AlphaAnimProgram a -> AlphaAnimProgram a
millAnim w t alpha = do
  nextCard <- headMay <$> Alpha.getDeck w
  final <- alpha
  Anim.mill w (fromMaybe strangeEnd nextCard) t
  return final

addToHandAnim :: WhichPlayer -> HandCard -> AlphaAnimProgram a -> AlphaAnimProgram a
addToHandAnim w c alpha = do
  handLength <- length <$> Alpha.getHand w
  final <- alpha
  if handLength < maxHandLength
    then Anim.draw w (TimeModifierOutQuint 1)
    else Anim.mill w (anyCard c) (TimeModifierOutQuint 1)
  return final

playAnim :: WhichPlayer -> HandCard -> Int -> AlphaAnimProgram a -> AlphaAnimProgram a
playAnim w c i alpha = do
  final <- alpha
  Anim.play w c i
  return final

transmuteAnim :: Wheel (Maybe Transmutation) -> AlphaAnimProgram a -> AlphaAnimProgram a
transmuteAnim transmutations alpha = do
  let activity = any isJust transmutations
  when activity (Anim.transmute transmutations)
  final <- alpha
  when activity Anim.null
  return final

transmuteActiveAnim :: Transmutation -> AlphaAnimProgram a -> AlphaAnimProgram a
transmuteActiveAnim transmutation alpha = do
  let transmutations = Wheel.init (\i -> if i == 0 then Just transmutation else Nothing)
  final <- alpha
  Anim.transmute transmutations
  return final

bounceAnim :: Wheel (Maybe CardBounce) -> TimeModifier -> AlphaAnimProgram a -> AlphaAnimProgram a
bounceAnim bounces t alpha = do
  let activity = any isJust bounces
  when activity (Anim.bounce bounces t)
  final <- alpha
  when activity Anim.null
  return final

moveStackAnim :: Wheel (Maybe Int) -> TimeModifier -> AlphaAnimProgram a -> AlphaAnimProgram a
moveStackAnim moves t alpha = do
  let activity = any isJust moves
  when activity (Anim.moveStack moves t)
  final <- alpha
  when activity Anim.null
  return final

discardStackAnim :: Wheel Bool -> AlphaAnimProgram a -> AlphaAnimProgram a
discardStackAnim discards alpha = do
  let activity = any id discards
  when activity (Anim.discardStack discards)
  final <- alpha
  when activity Anim.null
  return final

discardHandAnim :: WhichPlayer -> (Int -> Card -> Bool) -> AlphaAnimProgram a -> AlphaAnimProgram a
discardHandAnim w f alpha = do
  discards <- getHandDiscards w f
  let activity = any isDiscard discards
  final <- alpha
  when activity (Anim.discardHand w discards)
  return final

getHandDiscards :: WhichPlayer -> (Int -> Card -> Bool) -> AlphaAnimProgram [CardDiscard]
getHandDiscards w f = do
  hand <- Alpha.getHand w
  return $ getDiscards' 0 0 hand
  where
    getDiscards' :: Int -> Int -> [HandCard] -> [CardDiscard]
    getDiscards' handIndex finalHandIndex (card : rest) =
      if f handIndex $ anyCard card
        then CardDiscard card : getDiscards' (handIndex + 1) finalHandIndex rest
        else NoDiscard (knownCard card) finalHandIndex : getDiscards' (handIndex + 1) (finalHandIndex + 1) rest
    getDiscards' _ _ [] = []

revealAnim :: WhichPlayer -> (Int -> Card -> Bool) -> AlphaAnimProgram a -> AlphaAnimProgram a
revealAnim w f alpha = do
  hand <- Alpha.getHand w
  let initialReveals = isRevealed <$> hand :: [Bool]
  let reveals = uncurry f <$> zip [0 ..] (anyCard <$> hand) :: [Bool]
  final <- alpha
  let activity = any id $ uncurry xor <$> zip initialReveals reveals :: Bool
  when activity (Anim.reveal w reveals)
  return final


betaI :: ∀ a . Program a -> AlphaAnimProgram a
betaI = reinterpret2 (\x -> animI x $ raiseAlpha $ alphaI x)

data ExecDSL n where
  ExecAlpha :: Alpha.DSL a -> ExecDSL a
  ExecAnim :: Anim.DSL a -> ExecDSL a

execute :: ∀ a . Model -> AlphaAnimProgram a -> (Model, [ResolveData])
execute initialModel prog = (initialModel, []) 
    where
    execProg :: Eff '[ExecDSL, Alpha.DSL, Anim.DSL] a
    execProg = interpose toExecAnim $ interpose toExecAlpha $ reinterpret2 send prog
    initialState :: (Model, ModelDiff, [ResolveData])
    initialState = (initialModel, mempty, [])
    part_a :: Eff '[S.State (Model, ModelDiff, [ResolveData]), Alpha.DSL, Anim.DSL] a
    part_a = reinterpret go execProg
    -- part_b :: Eff '[Alpha.DSL, Anim.DSL] (Model, ModelDiff, [ResolveData])
    -- part_b = execState initialState $ part_a
    -- result :: (Model, ModelDiff, [ResolveData])
    -- result = run part_b
    toExecAlpha :: (Member ExecDSL effs) => Alpha.DSL b -> Eff effs b
    toExecAlpha = send . ExecAlpha
    toExecAnim :: (Member ExecDSL effs) => Anim.DSL b -> Eff effs b
    toExecAnim = send . ExecAnim
    go :: (Member (S.State (Model, ModelDiff, [ResolveData])) effs) => ∀ b . ExecDSL b -> Eff effs b
    go (ExecAlpha alpha) = do
      (model, diff, res :: [ResolveData]) <- S.get
      let (newDiff, next) = Alpha.alphaEffI model alpha
      let newModel = ModelDiff.update model newDiff
      S.put (newModel, diff <> newDiff, res)
      return next
    go (ExecAnim anim) = do
      -- newNext = if gameover model then return () else next anim
      (model :: Model, diff :: ModelDiff, res :: [ResolveData]) <- S.get
      let cardAnim = Anim.animate anim
      let damage = fromMaybe (0, 0) $ cardAnimDamage <$> cardAnim
      let resolveData = ResolveData diff cardAnim damage
      let newRes = res ++ [resolveData]
      S.put (model :: Model, mempty :: ModelDiff, res :: [ResolveData])
      return $ Anim.next anim
