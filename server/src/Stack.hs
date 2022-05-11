module Stack where

import Data.Foldable (toList)
import Data.Maybe (catMaybes, fromMaybe)
import StackCard (StackCard (..))
import Wheel (Wheel (..), indexWheel)
import qualified Wheel

type Stack = Wheel (Maybe StackCard)

init :: Stack
init = Wheel.init $ const Nothing

get :: Stack -> Int -> Maybe StackCard
get stack 0 = wheel_0 stack
get stack 1 = wheel_1 stack
get stack 2 = wheel_2 stack
get stack 3 = wheel_3 stack
get stack 4 = wheel_4 stack
get stack 5 = wheel_5 stack
get stack 6 = wheel_6 stack
get stack 7 = wheel_7 stack
get stack 8 = wheel_8 stack
get stack 9 = wheel_9 stack
get stack 10 = wheel_10 stack
get stack 11 = wheel_11 stack
get _ _ = Nothing

set :: Stack -> Int -> Maybe StackCard -> Stack
set stack 0 stackCard = stack {wheel_0 = stackCard}
set stack 1 stackCard = stack {wheel_1 = stackCard}
set stack 2 stackCard = stack {wheel_2 = stackCard}
set stack 3 stackCard = stack {wheel_3 = stackCard}
set stack 4 stackCard = stack {wheel_4 = stackCard}
set stack 5 stackCard = stack {wheel_5 = stackCard}
set stack 6 stackCard = stack {wheel_6 = stackCard}
set stack 7 stackCard = stack {wheel_7 = stackCard}
set stack 8 stackCard = stack {wheel_8 = stackCard}
set stack 9 stackCard = stack {wheel_9 = stackCard}
set stack 10 stackCard = stack {wheel_10 = stackCard}
set stack 11 stackCard = stack {wheel_11 = stackCard}
set stack _ _ = stack

rotate :: Stack -> Stack
rotate = Wheel.back

windup :: Stack -> Stack
windup = Wheel.fwrd

diasporaFromStack :: Stack -> [(Int, StackCard)]
diasporaFromStack stack =
  catMaybes $ fmap combine $ zip [0 ..] $ toList stack
  where
    combine :: (Int, Maybe StackCard) -> Maybe (Int, StackCard)
    combine (i, Just sc) = Just (i, sc)
    combine (_, Nothing) = Nothing

stackFromDiaspora :: [(Int, StackCard)] -> Stack
stackFromDiaspora diaspora =
  foldr reduce Stack.init diaspora
  where
    reduce :: (Int, StackCard) -> Stack -> Stack
    reduce (i, sc) stack = set stack i (Just sc)

diasporaLength :: Stack -> Int
diasporaLength = length . filter (\(i, _) -> i > 0) . diasporaFromStack

diasporaMap :: (Int -> StackCard -> Maybe a) -> Stack -> Wheel (Maybe a)
diasporaMap f stack = fForMaybes <$> indexWheel <*> stack
  where
    fForMaybes i (Just sc) = f i sc
    fForMaybes _ Nothing = Nothing

diasporaFilter :: (Int -> StackCard -> Bool) -> Stack -> Stack
diasporaFilter f stack =
  let maybeBoolWheel :: Wheel (Maybe Bool)
      maybeBoolWheel = (\i sc -> fmap (f i) sc) <$> indexWheel <*> stack
      boolWheel :: Wheel Bool
      boolWheel = fromMaybe False <$> maybeBoolWheel
      combine :: Bool -> Maybe StackCard -> Maybe StackCard
      combine True sc = sc
      combine False _ = Nothing
   in combine <$> boolWheel <*> stack
