module Stack where

import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Safe (atMay)
import StackCard (StackCard(..))
import Wheel (indexWheel, Wheel(..))

import qualified Wheel


type Stack = Wheel (Maybe StackCard)


init :: Stack
init = Wheel.init $ const Nothing


chainMask :: Stack -> Wheel Bool
chainMask s = chainMask' s falseMask
  where
    falseMask :: Wheel Bool
    falseMask = Wheel.init $ const False
    chainMask' :: Stack -> Wheel Bool -> Wheel Bool
    chainMask' stack mask =
      case wheel_0 stack of
        Just _ ->
          chainMask'
            (Wheel.back (stack { wheel_0 = Nothing } ))
            (Wheel.fwrd (mask { wheel_0 = True }))
        Nothing ->
          mask


chainLength :: Stack -> Int
chainLength stack = length $ filter id $ toList $ chainMask stack


chainMap :: (Int -> StackCard -> Maybe a) -> Stack -> Wheel (Maybe a)
chainMap f stack = chainOnlyF <$> chainMask stack <*> indexWheel <*> stack
  where
    chainOnlyF masked i mStackCard =
      case (mStackCard, masked) of
        (Just stackCard, True) ->
          f i stackCard
        _ ->
          Nothing


chainFilter :: (Int -> StackCard -> Bool) -> Stack -> Stack
chainFilter f = Stack.chainMap $ \i c -> if f i c then Just c else Nothing


chainToList :: Stack -> [StackCard]
chainToList stack =
  catMaybes $ snd <$> (filter fst $ zip (toList $ chainMask stack) (toList stack))


stackFromList :: [StackCard] -> Stack
stackFromList xs = Wheel.init (atMay xs)


modChain :: ([StackCard] -> [StackCard]) -> Stack -> Stack
modChain f stack = (<|>) <$> modded <*> stack
  where
    modded = stackFromList $ f $ chainToList stack :: Stack



rotate :: Stack -> Stack
rotate stack = Wheel.fwrd $ stack { wheel_0 = Nothing }

windup :: Stack -> Stack
windup = Wheel.back
