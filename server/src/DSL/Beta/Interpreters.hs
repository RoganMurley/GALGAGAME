{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module DSL.Beta.Interpreters where

import Bounce (CardBounce(..))
import Card (Card)
import CardAnim (Damage, Hurt, TimeModifier(..), cardAnimDamage)
import Control.Monad (when)
import Control.Monad.Free (Free(..), foldFree, liftF)
import Data.Functor.Sum (Sum(..))
import Data.Maybe (fromMaybe, isJust)
import Discard (CardDiscard(..), isDiscard)
import DSL.Beta.DSL
import DSL.Util (toLeft, toRight)
import HandCard (HandCard(..), anyCard, knownCard, isRevealed)
import Life (Life)
import Model (Model, gameover, maxHandLength)
import ModelDiff (ModelDiff)
import Player (WhichPlayer(..))
import ResolveData (ResolveData(..))
import Safe (headMay)
import Transmutation (Transmutation(..))
import Util (xor)
import Wheel (Wheel(..))

import qualified DSL.Alpha as Alpha
import qualified DSL.Anim as Anim
import qualified DSL.Log as Log
import qualified ModelDiff
import qualified Wheel

import {-# SOURCE #-} Cards (strangeEnd)


alphaI :: Program a -> Alpha.Program a
alphaI (Free (Raw p n))              = p                            >>  alphaI n
alphaI (Free (Hurt d w _ n))         = Alpha.hurt d w               >>  alphaI n
alphaI (Free (Heal h w n))           = Alpha.heal h w               >>  alphaI n
alphaI (Free (Draw w d _ n))         = Alpha.draw w d               >>  alphaI n
alphaI (Free (AddToHand w c n))      = Alpha.addToHand w c          >>  alphaI n
alphaI (Free (Play w c i n))         = Alpha.play w (anyCard c) i   >>  alphaI n
alphaI (Free (Transmute' t n))       = Alpha.transmute t            >>  alphaI n
alphaI (Free (TransmuteActive' t n)) = Alpha.transmuteActive t      >>  alphaI n
alphaI (Free (Rotate n))             = Alpha.rotate                 >>  alphaI n
alphaI (Free (Windup n))             = Alpha.windup                 >>  alphaI n
alphaI (Free (Bounce' f _ n))        = Alpha.bounce f               >>  alphaI n
alphaI (Free (DiscardStack' f n))    = Alpha.discardStack f         >>  alphaI n
alphaI (Free (DiscardHand w f n))    = Alpha.discardHand w f        >>  alphaI n
alphaI (Free (MoveStack' f _ n))     = Alpha.moveStack f            >>  alphaI n
alphaI (Free (Mill w _ n))           = Alpha.mill w                 >>  alphaI n
alphaI (Free (Reveal w f n))         = Alpha.reveal w f             >>  alphaI n
alphaI (Free (GetGen f))             = Alpha.getGen                 >>= alphaI . f
alphaI (Free (GetRot f))             = Alpha.getRot                 >>= alphaI . f
alphaI (Free (GetLife w f))          = Alpha.getLife w              >>= alphaI . f
alphaI (Free (GetHand w f))          = Alpha.getHand w              >>= alphaI . f
alphaI (Free (GetDeck w f))          = Alpha.getDeck w              >>= alphaI . f
alphaI (Free (GetStack f))           = Alpha.getStack               >>= alphaI . f
alphaI (Free (GetHold f))            = Alpha.getHold                >>= alphaI . f
alphaI (Free (GetModel f))           = Alpha.getModel               >>= alphaI . f
alphaI (Free (RawAnim _ n))          = alphaI n
alphaI (Free (Null n))               = alphaI n
alphaI (Pure x)                      = Pure x


basicAnim :: Anim.DSL () -> Alpha.Program a -> AlphaAnimProgram a
basicAnim anim alphaProgram = toLeft alphaProgram <* (toRight . liftF $ anim)


animI :: DSL a -> (Alpha.Program a -> AlphaAnimProgram a)
animI (Null _)               = basicAnim $ Anim.Null ()
animI (Rotate _)             = basicAnim $ Anim.Rotate ()
animI (Windup _)             = basicAnim $ Anim.Windup ()
animI (RawAnim r _)          = basicAnim $ Anim.Raw r ()
animI (GetGen _)             = basicAnim $ Anim.GetGen ()
animI (Hurt d w h _)         = damageAnim d w h
animI (Heal _ w _)           = healAnim w
animI (AddToHand w c  _)     = addToHandAnim w c
animI (Draw w d t _)         = drawAnim w d t
animI (Play w c i _)         = playAnim w c i
animI (Transmute' t _)       = transmuteAnim t
animI (TransmuteActive' t _) = transmuteActiveAnim t
animI (Bounce' b t _)        = bounceAnim b t
animI (DiscardStack' d _)    = discardStackAnim d
animI (DiscardHand w f _)    = discardHandAnim w f
animI (MoveStack' m t _)     = moveStackAnim m t
animI (Mill w t _)           = millAnim w t
animI (Reveal w f _)         = revealAnim w f
animI _                      = toLeft


damageAnim :: Life -> WhichPlayer -> Hurt -> Alpha.Program a -> AlphaAnimProgram a
damageAnim d w h alpha = do
  final <- toLeft alpha
  when (d > 0) $ toRight . liftF $ Anim.Hurt w d h ()
  return final


healAnim :: WhichPlayer -> Alpha.Program a -> AlphaAnimProgram a
healAnim w alpha = do
  oldLife <- toLeft $ Alpha.getLife w
  final <- toLeft alpha
  newLife <- toLeft $ Alpha.getLife w
  let lifeChange = newLife - oldLife
  when (lifeChange > 0) $ toRight . liftF $ Anim.Heal w lifeChange ()
  return final


drawAnim :: WhichPlayer -> WhichPlayer -> TimeModifier -> Alpha.Program a -> AlphaAnimProgram a
drawAnim w d t alpha = do
  nextCard <- headMay <$> toLeft (Alpha.getDeck d)
  handLength <- length <$> toLeft (Alpha.getHand w)
  final <- toLeft alpha
  if (handLength < maxHandLength)
    then toRight . liftF $ Anim.Draw w t ()
    else toRight . liftF $ Anim.Mill w (fromMaybe strangeEnd nextCard) (TimeModifierOutQuint 1) ()
  return final


millAnim :: WhichPlayer -> TimeModifier -> Alpha.Program a -> AlphaAnimProgram a
millAnim w t alpha = do
  nextCard <- headMay <$> toLeft (Alpha.getDeck w)
  final <- toLeft alpha
  toRight . liftF $ Anim.Mill w (fromMaybe strangeEnd nextCard) t ()
  return final


addToHandAnim :: WhichPlayer -> HandCard -> Alpha.Program a -> AlphaAnimProgram a
addToHandAnim w c alpha = do
  handLength <- length <$> toLeft (Alpha.getHand w)
  final <- toLeft alpha
  if (handLength < maxHandLength)
    then toRight . liftF $ Anim.Draw w (TimeModifierOutQuint 1) ()
    else toRight . liftF $ Anim.Mill w (anyCard c) (TimeModifierOutQuint 1) ()
  return final


playAnim :: WhichPlayer -> HandCard -> Int -> Alpha.Program a -> AlphaAnimProgram a
playAnim w c i alpha = do
  final <- toLeft alpha
  toRight . liftF $ Anim.Play w c i ()
  return final


transmuteAnim :: Wheel (Maybe Transmutation) -> Alpha.Program a -> AlphaAnimProgram a
transmuteAnim transmutations alpha = do
  let activity = any isJust transmutations
  when activity (toRight . liftF $ Anim.Transmute transmutations ())
  final <- toLeft alpha
  when activity (toRight . liftF $ Anim.Null ())
  return final


transmuteActiveAnim :: Transmutation -> Alpha.Program a -> AlphaAnimProgram a
transmuteActiveAnim transmutation alpha = do
  let transmutations = Wheel.init (\i -> if i == 0 then Just transmutation else Nothing)
  final <- toLeft alpha
  toRight . liftF $ Anim.Transmute transmutations ()
  return final


bounceAnim :: Wheel (Maybe CardBounce) -> TimeModifier -> Alpha.Program a -> AlphaAnimProgram a
bounceAnim bounces t alpha = do
  let activity = any isJust bounces
  when activity (toRight . liftF $ Anim.Bounce bounces t ())
  final <- toLeft alpha
  when activity (toRight . liftF $ Anim.Null ())
  return final


moveStackAnim :: Wheel (Maybe Int) -> TimeModifier -> Alpha.Program a -> AlphaAnimProgram a
moveStackAnim moves t alpha = do
  let activity = any isJust moves
  when activity (toRight . liftF $ Anim.MoveStack moves t ())
  final <- toLeft alpha
  when activity (toRight . liftF $ Anim.Null ())
  return final


discardStackAnim :: Wheel Bool -> Alpha.Program a -> AlphaAnimProgram a
discardStackAnim discards alpha = do
  let activity = any id discards
  when activity (toRight . liftF $ Anim.DiscardStack discards ())
  final <- toLeft alpha
  when activity (toRight . liftF $ Anim.Null ())
  return final


discardHandAnim :: WhichPlayer -> (Int -> Card -> Bool) -> Alpha.Program a -> AlphaAnimProgram a
discardHandAnim w f alpha = do
  discards <- toLeft $ getHandDiscards w f
  let activity = any isDiscard discards
  final <- toLeft alpha
  when activity (toRight . liftF $ Anim.DiscardHand w discards ())
  return final


getHandDiscards :: WhichPlayer -> (Int -> Card -> Bool) -> Alpha.Program [CardDiscard]
getHandDiscards w f = do
  hand <- Alpha.getHand w
  return $ getDiscards' 0 0 hand
    where
      getDiscards' :: Int -> Int -> [HandCard] -> [CardDiscard]
      getDiscards' handIndex finalHandIndex (card:rest) =
        if f handIndex $ anyCard card then
          CardDiscard card : getDiscards' (handIndex + 1) finalHandIndex rest
        else
          NoDiscard (knownCard card) finalHandIndex : getDiscards' (handIndex + 1) (finalHandIndex + 1) rest
      getDiscards' _ _ [] = []


revealAnim :: WhichPlayer -> (Int -> Card -> Bool) -> Alpha.Program a -> AlphaAnimProgram a
revealAnim w f alpha = do
  hand <- toLeft $ Alpha.getHand w
  let initialReveals = isRevealed <$> hand :: [Bool]
  let reveals = uncurry f <$> zip [0..] (anyCard <$> hand) :: [Bool]
  final <- toLeft alpha
  let activity = any id $ (uncurry xor) <$> zip initialReveals reveals :: Bool
  when activity (toRight . liftF $ Anim.Reveal w reveals ())
  return final


type AlphaAnimProgram = Free (Sum Alpha.DSL Anim.DSL)
type AlphaLogAnimProgram = Free (Sum (Sum Alpha.DSL Log.DSL) Anim.DSL)


liftAlphaAnim :: ∀ a . Sum Alpha.DSL Anim.DSL a -> AlphaLogAnimProgram a
liftAlphaAnim (InL alpha) = toLeft $ Alpha.decorateLog alpha
liftAlphaAnim (InR anim)  = toRight $ liftF anim


betaI :: ∀ a . DSL a -> AlphaLogAnimProgram a
betaI x = (foldFree liftAlphaAnim) . (animI x) . alphaI $ liftF x


execute :: Model -> AlphaLogAnimProgram () -> (Model, String, [ResolveData])
execute = execute' "" [] mempty
  where
    execute' :: String -> [ResolveData] -> ModelDiff -> Model -> AlphaLogAnimProgram () -> (Model, String, [ResolveData])

    execute' l a _ m (Pure _) =
      (m, l, a)

    execute' l a d m (Free (InR anim)) =
      let
        next = if gameover m then Pure () else Anim.next anim
        cardAnim = Anim.animate anim
        damage = fromMaybe (0, 0) $ cardAnimDamage <$> cardAnim
        resolveData = ResolveData d cardAnim damage
      in
        execute' l (a ++ [resolveData]) mempty m next

    execute' l a d m (Free (InL (InL p))) =
      let
         (newDiff, n) = Alpha.alphaEffI m p
         newModel = ModelDiff.update m newDiff
      in
        execute' l a (d <> newDiff) newModel n

    execute' l a d m (Free (InL (InR (Log.Log l' n)))) =
      execute' (l ++ l' ++ "\n") a d m n


damageNumbersI :: Model -> Program () -> (Damage, Damage)
damageNumbersI model program =
  let
    (_, _, resolveData) = execute model $ foldFree betaI program
    damage = resolveData_animDamage <$> resolveData :: [(Damage, Damage)]
    damagePa = sum $ fst <$> damage :: Damage
    damagePb = sum $ snd <$> damage :: Damage
  in
    (damagePa, damagePb)
