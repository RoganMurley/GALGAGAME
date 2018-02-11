module DSL.Util where

import Control.Monad.Free (Free(..))
import Data.Functor.Sum (Sum(..))


toLeft :: (Functor f, Functor g) => Free f a -> Free (Sum f g) a
toLeft (Free f) = Free $ toLeft <$> InL f
toLeft (Pure x) = Pure x


toRight :: (Functor f, Functor g) => Free g a -> Free (Sum f g) a
toRight (Free f) = Free $ toRight <$> InR f
toRight (Pure x) = Pure x
