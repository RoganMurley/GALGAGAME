module Stack where

import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Safe (atMay)
import StackCard (StackCard(..))
import Wheel (indexWheel, Wheel(..))

import Debug.Trace (trace)

import qualified Wheel


type Stack = Wheel (Maybe StackCard)


init :: Stack
init = Wheel.init $ const Nothing


get :: Stack -> Int -> Maybe StackCard
get stack 0  = wheel_0  stack
get stack 1  = wheel_1  stack
get stack 2  = wheel_2  stack
get stack 3  = wheel_3  stack
get stack 4  = wheel_4  stack
get stack 5  = wheel_5  stack
get stack 6  = wheel_6  stack
get stack 7  = wheel_7  stack
get stack 8  = wheel_8  stack
get stack 9  = wheel_9  stack
get stack 10 = wheel_10 stack
get stack 11 = wheel_11 stack
get _     _  = Nothing


set :: Stack -> Int -> Maybe StackCard -> Stack
set stack 0  stackCard = stack { wheel_0  = stackCard }
set stack 1  stackCard = stack { wheel_1  = stackCard }
set stack 2  stackCard = stack { wheel_2  = stackCard }
set stack 3  stackCard = stack { wheel_3  = stackCard }
set stack 4  stackCard = stack { wheel_4  = stackCard }
set stack 5  stackCard = stack { wheel_5  = stackCard }
set stack 6  stackCard = stack { wheel_6  = stackCard }
set stack 7  stackCard = stack { wheel_7  = stackCard }
set stack 8  stackCard = stack { wheel_8  = stackCard }
set stack 9  stackCard = stack { wheel_9  = stackCard }
set stack 10 stackCard = stack { wheel_10 = stackCard }
set stack 11 stackCard = stack { wheel_11 = stackCard }
set stack _  _         = stack


chainMask :: Stack -> Wheel Bool
chainMask s = chainMask' s $ Wheel.init $ const False
  where
    chainMask' :: Stack -> Wheel Bool -> Wheel Bool
    chainMask' stack mask =
      trace ("mask: " ++ show mask) $
      case wheel_0 stack of
        Just _ ->
          chainMask'
            (Wheel.back (stack { wheel_0 = Nothing } ))
            (Wheel.fwrd (mask { wheel_11 = True }))
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
