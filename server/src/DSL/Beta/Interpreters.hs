{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module DSL.Beta.Interpreters where

import Bounce (CardBounce(..))
import Card (Card)
import CardAnim (cardAnimDamage)
import Control.Monad.Free (Free(..), foldFree, liftF)
import Data.Functor.Sum (Sum(..))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Discard (CardDiscard(..))
import DSL.Beta.DSL
import DSL.Util (toLeft, toRight)
import Life (Life)
import Model (Model, gameover, maxHandLength)
import ModelDiff (ModelDiff)
import Player (WhichPlayer(..))
import ResolveData (ResolveData(..))
import Safe (headMay)
import Stack (chainMap)
import StackCard (StackCard(..))
import Transmutation
import Wheel

import qualified DSL.Alpha as Alpha
import qualified DSL.Anim as Anim
import qualified DSL.Log as Log
import qualified ModelDiff

import {-# SOURCE #-} Cards (strangeEnd)


alphaI :: Program a -> Alpha.Program a
alphaI (Free (Raw p n))           = p                      >>  alphaI n
alphaI (Free (Hurt d w _ n))      = Alpha.hurt d w         >>  alphaI n
alphaI (Free (Heal h w n))        = Alpha.heal h w         >>  alphaI n
alphaI (Free (Draw w d n))        = Alpha.draw w d         >>  alphaI n
alphaI (Free (AddToHand w c n))   = Alpha.addToHand w c    >>  alphaI n
alphaI (Free (Confound n))        = Alpha.confound         >>  alphaI n
alphaI (Free (Reverse n))         = Alpha.reversal         >>  alphaI n
alphaI (Free (Play w c i n))      = Alpha.play w c i       >>  alphaI n
alphaI (Free (Transmute f n))     = Alpha.transmute f      >>  alphaI n
alphaI (Free (Rotate n))          = Alpha.rotate           >>  alphaI n
alphaI (Free (Windup n))          = Alpha.windup           >>  alphaI n
alphaI (Free (Fabricate c n))     = Alpha.fabricate c      >>  alphaI n
alphaI (Free (Bounce f n))        = Alpha.bounce f         >>  alphaI n
alphaI (Free (DiscardStack f n))  = Alpha.discardStack f   >>  alphaI n
alphaI (Free (DiscardHand w f n)) = Alpha.discardHand w f  >>  alphaI n
alphaI (Free (GetGen f))          = Alpha.getGen           >>= alphaI . f
alphaI (Free (GetRot f))          = Alpha.getRot           >>= alphaI . f
alphaI (Free (GetLife w f))       = Alpha.getLife w        >>= alphaI . f
alphaI (Free (GetHand w f))       = Alpha.getHand w        >>= alphaI . f
alphaI (Free (GetDeck w f))       = Alpha.getDeck w        >>= alphaI . f
alphaI (Free (GetStack f))        = Alpha.getStack         >>= alphaI . f
alphaI (Free (RawAnim _ n))       = alphaI n
alphaI (Free (Null n))            = alphaI n
alphaI (Pure x)                   = Pure x


basicAnim :: Anim.DSL () -> Alpha.Program a -> AlphaAnimProgram a
basicAnim anim alphaProgram = toLeft alphaProgram <* (toRight . liftF $ anim)


animI :: DSL a -> (Alpha.Program a -> AlphaAnimProgram a)
animI (Null _)            = basicAnim $ Anim.Null ()
animI (Hurt d w h _)      = basicAnim $ Anim.Hurt w d h ()
animI (Confound _)        = basicAnim $ Anim.Confound ()
animI (Reverse _)         = basicAnim $ Anim.Reverse ()
animI (Rotate _)          = basicAnim $ Anim.Rotate ()
animI (Windup _)          = basicAnim $ Anim.Windup ()
animI (Fabricate c _)     = basicAnim $ Anim.Fabricate c ()
animI (RawAnim r _)       = basicAnim $ Anim.Raw r ()
animI (Heal _ w _)        = healAnim w
animI (AddToHand w c  _)  = addToHandAnim w c
animI (Draw w d _)        = drawAnim w d
animI (Play w c i _)      = playAnim w c i
animI (Transmute f _)     = transmuteAnim f
animI (Bounce f _)        = bounceAnim f
animI (DiscardStack f _)  = discardStackAnim f
animI (DiscardHand w f _) = discardHandAnim w f
animI _                   = toLeft


healAnim :: WhichPlayer -> Alpha.Program a -> AlphaAnimProgram a
healAnim w alpha = do
  oldLife <- toLeft $ Alpha.getLife w
  final <- toLeft alpha
  newLife <- toLeft $ Alpha.getLife w
  let lifeChange = newLife - oldLife
  toRight . liftF $ Anim.Heal w lifeChange ()
  return final


drawAnim :: WhichPlayer -> WhichPlayer -> Alpha.Program a -> AlphaAnimProgram a
drawAnim w d alpha = do
  nextCard <- headMay <$> toLeft (Alpha.getDeck d)
  handLength <- length <$> toLeft (Alpha.getHand w)
  final <- toLeft alpha
  if (handLength < maxHandLength)
    then toRight . liftF $ Anim.Draw w ()
    else toRight . liftF $ Anim.Mill w (fromMaybe strangeEnd nextCard) ()
  return final


addToHandAnim :: WhichPlayer -> Card -> Alpha.Program a -> AlphaAnimProgram a
addToHandAnim w c alpha = do
  handLength <- length <$> toLeft (Alpha.getHand w)
  final <- toLeft alpha
  if (handLength < maxHandLength)
    then toRight . liftF $ Anim.Draw w ()
    else toRight . liftF $ Anim.Mill w c ()
  return final


playAnim :: WhichPlayer -> Card -> Int -> Alpha.Program a -> AlphaAnimProgram a
playAnim w c i alpha = do
  final <- toLeft alpha
  toRight . liftF $ Anim.Play w c i ()
  return final


transmuteAnim :: (Int -> StackCard -> Maybe Transmutation) -> Alpha.Program a -> AlphaAnimProgram a
transmuteAnim f alpha = do
  stack <- toLeft Alpha.getStack
  let transmutations = chainMap f stack
  final <- toLeft alpha
  toRight . liftF $ Anim.Transmute transmutations ()
  return final


bounceAnim :: (Int -> StackCard -> Bool) -> Alpha.Program a -> AlphaAnimProgram a
bounceAnim f alpha = do
  bounces <- toLeft $ getBounces f
  toRight . liftF $ Anim.Bounce bounces ()
  final <- toLeft alpha
  toRight . liftF $ Anim.Null ()
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
  let BounceState{ bounces } = foldr reduce startState stack
  return bounces
  where
    reduce :: Maybe StackCard -> BounceState -> BounceState
    reduce Nothing state =
      state
        { stackIndex = stackIndex state + 1
        , bounces = Wheel.fwrd $ bounces state
        }
    reduce (Just stackCard) state@(BounceState{ stackIndex, handAIndex, handBIndex, bounces }) =
      if f stackIndex stackCard then
        case stackcard_owner stackCard of
          PlayerA ->
            if handAIndex >= maxHandLength then
              (state
                { stackIndex = stackIndex + 1
                , bounces = Wheel.fwrd $ bounces { wheel_0 = Just BounceDiscard }
                }
              )
            else
              (state
                { stackIndex = stackIndex + 1
                , handAIndex = handAIndex + 1
                , bounces = Wheel.fwrd $ bounces { wheel_0 = Just (BounceIndex handAIndex) }
                }
              )
          PlayerB ->
            if handBIndex >= maxHandLength then
              (state
                { stackIndex = stackIndex + 1
                , bounces = Wheel.fwrd $ bounces { wheel_0 = Just BounceDiscard }
                }
              )
            else
              (state
                { stackIndex = stackIndex + 1
                , handBIndex = handBIndex + 1
                , bounces = Wheel.fwrd $ bounces { wheel_0 = Just (BounceIndex handBIndex) }
                }
              )
      else
        (state
          { stackIndex = stackIndex + 1
          , bounces = Wheel.fwrd bounces
          }
        )


discardStackAnim :: (Int -> StackCard -> Bool) -> Alpha.Program a -> AlphaAnimProgram a
discardStackAnim f alpha = do
  discards <- toLeft $ getStackDiscards f
  toRight . liftF $ Anim.DiscardStack discards ()
  final <- toLeft alpha
  toRight . liftF $ Anim.Null ()
  return final


discardHandAnim :: WhichPlayer -> (Int -> Card -> Bool) -> Alpha.Program a -> AlphaAnimProgram a
discardHandAnim w f alpha = do
  discards <- toLeft $ getHandDiscards w f
  toRight . liftF $ Anim.DiscardHand w discards ()
  final <- toLeft alpha
  toRight . liftF $ Anim.Null ()
  return final


getHandDiscards :: WhichPlayer -> (Int -> Card -> Bool) -> Alpha.Program [CardDiscard]
getHandDiscards w f = do
  hand <- Alpha.getHand w
  return $ getDiscards' 0 0 $ uncurry f <$> zip [0..] hand
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
  let discarder = \i c -> if f i c then Just True else Just False
  return $ fromMaybe False <$> Stack.chainMap discarder stack


type AlphaAnimProgram = Free (Sum Alpha.DSL Anim.DSL)
type AlphaLogAnimProgram = Free (Sum (Sum Alpha.DSL Log.DSL) Anim.DSL)


liftAlphaAnim :: ∀ a . Sum Alpha.DSL Anim.DSL a -> AlphaLogAnimProgram a
liftAlphaAnim (InL alpha) = toLeft $ Alpha.decorateLog alpha
liftAlphaAnim (InR anim)  = toRight $ liftF anim


betaI :: ∀ a . DSL a -> AlphaLogAnimProgram a
betaI x = (foldFree liftAlphaAnim) . (animI x) . alphaI $ liftF x


execute :: Model -> Maybe StackCard -> AlphaLogAnimProgram () -> (Model, String, [ResolveData])
execute = execute' "" [] mempty
  where
    execute' :: String -> [ResolveData] -> ModelDiff -> Model -> Maybe StackCard -> AlphaLogAnimProgram () -> (Model, String, [ResolveData])

    execute' l a _ m _ (Pure _) =
      (m, l, a)

    execute' l a d m s (Free (InR anim)) =
      let
        next = if gameover m then Pure () else Anim.next anim
        cardAnim = Anim.animate anim
        damage = fromMaybe (0, 0) $ cardAnimDamage <$> cardAnim
        resolveData = ResolveData d cardAnim damage s
      in
        execute' l (a ++ [resolveData]) mempty m s next

    execute' l a d m s (Free (InL (InL p))) =
      let
         (newDiff, n) = Alpha.alphaEffI m p
         newModel = ModelDiff.update m newDiff
      in
        execute' l a (d <> newDiff) newModel s n

    execute' l a d m s (Free (InL (InR (Log.Log l' n)))) =
      execute' (l ++ l' ++ "\n") a d m s n


damageNumbersI :: Model -> Program () -> (Life, Life)
damageNumbersI model program =
  let
    (_, _, resolveData) = execute model Nothing $ foldFree betaI program
    damage = resolveData_animDamage <$> resolveData :: [(Life, Life)]
    damagePa = sum $ fst <$> damage :: Life
    damagePb = sum $ snd <$> damage :: Life
  in
    (damagePa, damagePb)
