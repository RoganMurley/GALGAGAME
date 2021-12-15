{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module DSL.Beta.Interpreters where

import Bounce (CardBounce(..))
import Card (Card)
import CardAnim (Damage, Hurt, cardAnimDamage)
import Control.Monad (when)
import Control.Monad.Free (Free(..), foldFree, liftF)
import Data.Foldable (foldl')
import Data.Functor.Sum (Sum(..))
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid ((<>))
import Discard (CardDiscard(..), isDiscard)
import DSL.Beta.DSL
import DSL.Util (toLeft, toRight)
import HandCard (HandCard(..), anyCard)
import Life (Life)
import Model (Model, gameover, maxHandLength)
import ModelDiff (ModelDiff)
import Player (WhichPlayer(..))
import ResolveData (ResolveData(..))
import Safe (headMay)
import StackCard (StackCard(..))
import Transmutation
import Wheel

import qualified DSL.Alpha as Alpha
import qualified DSL.Anim as Anim
import qualified DSL.Log as Log
import qualified ModelDiff
import qualified Stack

import {-# SOURCE #-} Cards (strangeEnd)


alphaI :: Program a -> Alpha.Program a
alphaI (Free (Raw p n))             = p                            >>  alphaI n
alphaI (Free (Hurt d w _ n))        = Alpha.hurt d w               >>  alphaI n
alphaI (Free (Heal h w n))          = Alpha.heal h w               >>  alphaI n
alphaI (Free (Draw w d _ n))        = Alpha.draw w d               >>  alphaI n
alphaI (Free (AddToHand w c n))     = Alpha.addToHand w c          >>  alphaI n
alphaI (Free (Play w c i n))        = Alpha.play w (anyCard c) i   >>  alphaI n
alphaI (Free (Transmute f n))       = Alpha.transmute f            >>  alphaI n
alphaI (Free (TransmuteActive f n)) = Alpha.transmuteActive f      >>  alphaI n
alphaI (Free (Rotate n))            = Alpha.rotate                 >>  alphaI n
alphaI (Free (Windup n))            = Alpha.windup                 >>  alphaI n
alphaI (Free (Bounce f n))          = Alpha.bounce f               >>  alphaI n
alphaI (Free (DiscardStack f n))    = Alpha.discardStack f         >>  alphaI n
alphaI (Free (DiscardHand w f n))   = Alpha.discardHand w f        >>  alphaI n
alphaI (Free (MoveStack f _ n))     = Alpha.moveStack f            >>  alphaI n
alphaI (Free (Mill w _ n))          = Alpha.mill w                 >>  alphaI n
alphaI (Free (GetGen f))            = Alpha.getGen                 >>= alphaI . f
alphaI (Free (GetRot f))            = Alpha.getRot                 >>= alphaI . f
alphaI (Free (GetLife w f))         = Alpha.getLife w              >>= alphaI . f
alphaI (Free (GetHand w f))         = Alpha.getHand w              >>= alphaI . f
alphaI (Free (GetDeck w f))         = Alpha.getDeck w              >>= alphaI . f
alphaI (Free (GetStack f))          = Alpha.getStack               >>= alphaI . f
alphaI (Free (GetHold f))           = Alpha.getHold                >>= alphaI . f
alphaI (Free (GetModel f))          = Alpha.getModel               >>= alphaI . f
alphaI (Free (RawAnim _ n))         = alphaI n
alphaI (Free (Null n))              = alphaI n
alphaI (Pure x)                     = Pure x


basicAnim :: Anim.DSL () -> Alpha.Program a -> AlphaAnimProgram a
basicAnim anim alphaProgram = toLeft alphaProgram <* (toRight . liftF $ anim)


animI :: DSL a -> (Alpha.Program a -> AlphaAnimProgram a)
animI (Null _)              = basicAnim $ Anim.Null ()
animI (Rotate _)            = basicAnim $ Anim.Rotate ()
animI (Windup _)            = basicAnim $ Anim.Windup ()
animI (RawAnim r _)         = basicAnim $ Anim.Raw r ()
animI (GetGen _)            = basicAnim $ Anim.GetGen ()
animI (Hurt d w h _)        = damageAnim d w h
animI (Heal _ w _)          = healAnim w
animI (AddToHand w c  _)    = addToHandAnim w c
animI (Draw w d t _)        = drawAnim w d t
animI (Play w c i _)        = playAnim w c i
animI (Transmute f _)       = transmuteAnim f
animI (TransmuteActive f _) = transmuteActiveAnim f
animI (Bounce f _)          = bounceAnim f
animI (DiscardStack f _)    = discardStackAnim f
animI (DiscardHand w f _)   = discardHandAnim w f
animI (MoveStack f t _)     = moveStackAnim f t
animI (Mill w t _)          = millAnim w t
animI _                     = toLeft


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


drawAnim :: WhichPlayer -> WhichPlayer -> Float -> Alpha.Program a -> AlphaAnimProgram a
drawAnim w d t alpha = do
  nextCard <- headMay <$> toLeft (Alpha.getDeck d)
  handLength <- length <$> toLeft (Alpha.getHand w)
  final <- toLeft alpha
  if (handLength < maxHandLength)
    then toRight . liftF $ Anim.Draw w t ()
    else toRight . liftF $ Anim.Mill w (fromMaybe strangeEnd nextCard) 1 ()
  return final


millAnim :: WhichPlayer -> Float -> Alpha.Program a -> AlphaAnimProgram a
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
    then toRight . liftF $ Anim.Draw w 1 ()
    else toRight . liftF $ Anim.Mill w (anyCard c) 1 ()
  return final


playAnim :: WhichPlayer -> HandCard -> Int -> Alpha.Program a -> AlphaAnimProgram a
playAnim w c i alpha = do
  final <- toLeft alpha
  toRight . liftF $ Anim.Play w c i ()
  return final


transmuteAnim :: (Int -> StackCard -> Maybe Transmutation) -> Alpha.Program a -> AlphaAnimProgram a
transmuteAnim f alpha = do
  stack <- toLeft Alpha.getStack
  let transmutations = Stack.diasporaMap f stack
  let activity = any isJust transmutations
  when activity (toRight . liftF $ Anim.Transmute transmutations ())
  final <- toLeft alpha
  when activity (toRight . liftF $ Anim.Null ())
  return final


transmuteActiveAnim :: (StackCard -> Maybe StackCard) -> Alpha.Program a -> AlphaAnimProgram a
transmuteActiveAnim f alpha = do
  stack <- toLeft Alpha.getStack
  let mActiveCard = wheel_0 stack
  case mActiveCard of
    Just activeCard ->
      case f activeCard of
        Just finalCard -> do
          let transmutations = Wheel.init (\i -> if i == 0 then Just (Transmutation activeCard finalCard) else Nothing)
          final <- toLeft alpha
          toRight . liftF $ Anim.Transmute transmutations ()
          return final
        Nothing -> do
          final <- toLeft alpha
          return final
    Nothing -> do
      final <- toLeft alpha
      return final


bounceAnim :: (Int -> StackCard -> Bool) -> Alpha.Program a -> AlphaAnimProgram a
bounceAnim f alpha = do
  bounces <- toLeft $ getBounces f
  let activity = any isJust bounces
  when activity (toRight . liftF $ Anim.Bounce bounces ())
  final <- toLeft alpha
  when activity (toRight . liftF $ Anim.Null ())
  return final


moveStackAnim :: (Int -> StackCard -> Maybe Int) -> Int -> Alpha.Program a -> AlphaAnimProgram a
moveStackAnim f time alpha = do
  stack <- toLeft Alpha.getStack
  let moves = Stack.diasporaMap f stack
  let activity = any isJust moves
  when activity (toRight . liftF $ Anim.MoveStack moves time ())
  final <- toLeft alpha
  when activity (toRight . liftF $ Anim.Null ())
  return final



data BounceState = BounceState
  { stackIndex :: Int
  , handAIndex :: Int
  , handBIndex :: Int
  , bounces    :: Wheel (Maybe CardBounce)
  } deriving (Show)


getBounces :: (Int -> StackCard -> Bool) -> Alpha.Program (Wheel (Maybe CardBounce))
getBounces f = do
  stack <- Alpha.getStack
  handALen <- length <$> Alpha.getHand PlayerA
  handBLen <- length <$> Alpha.getHand PlayerB
  let startState = (BounceState{
    stackIndex = 0,
    handAIndex = handALen,
    handBIndex = handBLen,
    bounces = Wheel.init $ const Nothing
  })
  let BounceState{ bounces } = foldl' reduce startState stack
  return bounces
  where
    next :: BounceState -> BounceState
    next state@(BounceState{ stackIndex, bounces }) = (
      state
        { stackIndex = stackIndex + 1
        , bounces = Wheel.back bounces
        }
      )
    reduce :: BounceState -> Maybe StackCard -> BounceState
    reduce state Nothing = next state
    reduce state@(BounceState{ stackIndex, handAIndex, handBIndex, bounces }) (Just stackCard) =
      next $
        if f stackIndex stackCard then
          case stackcard_owner stackCard of
            PlayerA ->
              if handAIndex >= maxHandLength then
                (state { bounces = bounces { wheel_0 = Just BounceDiscard } })
              else
                (state
                  { handAIndex = handAIndex + 1
                  , bounces = bounces { wheel_0 = Just (BounceIndex stackIndex handAIndex) }
                  }
                )
            PlayerB ->
              if handBIndex >= maxHandLength then
                (state{ bounces = bounces { wheel_0 = Just BounceDiscard } })
              else
                (state
                  { handBIndex = handBIndex + 1
                  , bounces = bounces { wheel_0 = Just (BounceIndex stackIndex handBIndex) }
                  }
                )
        else
          state


discardStackAnim :: (Int -> StackCard -> Bool) -> Alpha.Program a -> AlphaAnimProgram a
discardStackAnim f alpha = do
  discards <- toLeft $ getStackDiscards f
  let activity = any id discards
  when activity (toRight . liftF $ Anim.DiscardStack discards ())
  final <- toLeft alpha
  when activity (toRight . liftF $ Anim.Null ())
  return final


discardHandAnim :: WhichPlayer -> (Int -> Card -> Bool) -> Alpha.Program a -> AlphaAnimProgram a
discardHandAnim w f alpha = do
  discards <- toLeft $ getHandDiscards w f
  let activity = any isDiscard discards
  when activity (toRight . liftF $ Anim.DiscardHand w discards ())
  final <- toLeft alpha
  when activity (toRight . liftF $ Anim.Null ())
  return final


getHandDiscards :: WhichPlayer -> (Int -> Card -> Bool) -> Alpha.Program [CardDiscard]
getHandDiscards w f = do
  hand <- Alpha.getHand w
  return $ getDiscards' 0 0 $ (\(index, card) -> f index (anyCard card)) <$> zip [0..] hand
    where
      getDiscards' :: Int -> Int -> [Bool] -> [CardDiscard]
      getDiscards' handIndex finalHandIndex (doDiscard:rest) =
        if doDiscard then
          CardDiscard : getDiscards' (handIndex + 1) finalHandIndex rest
        else
          NoDiscard finalHandIndex : getDiscards' (handIndex + 1) (finalHandIndex + 1) rest
      getDiscards' _ _ [] = []


getStackDiscards :: (Int -> StackCard -> Bool) -> Alpha.Program (Wheel Bool)
getStackDiscards f = do
  stack <- Alpha.getStack
  let discarder = \i c -> Just $ f i c
  return $ fromMaybe False <$> Stack.diasporaMap discarder stack


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
