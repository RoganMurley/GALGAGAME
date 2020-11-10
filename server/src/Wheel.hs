module Wheel where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Semigroup ((<>), Semigroup)
import Mirror (Mirror(..))


-- Wheel
data Wheel a = Wheel
  { wheel_0  :: a
  , wheel_1  :: a
  , wheel_2  :: a
  , wheel_3  :: a
  , wheel_4  :: a
  , wheel_5  :: a
  , wheel_6  :: a
  , wheel_7  :: a
  , wheel_8  :: a
  , wheel_9  :: a
  , wheel_10 :: a
  , wheel_11 :: a
  }
  deriving (Eq, Show)


instance Functor Wheel where
  fmap f w = Wheel
    { wheel_0  = f $ wheel_0 w
    , wheel_1  = f $ wheel_1 w
    , wheel_2  = f $ wheel_2 w
    , wheel_3  = f $ wheel_3 w
    , wheel_4  = f $ wheel_4 w
    , wheel_5  = f $ wheel_5 w
    , wheel_6  = f $ wheel_6 w
    , wheel_7  = f $ wheel_7 w
    , wheel_8  = f $ wheel_8 w
    , wheel_9  = f $ wheel_9 w
    , wheel_10 = f $ wheel_10 w
    , wheel_11 = f $ wheel_11 w
    }

instance (Mirror a) => Mirror (Wheel a) where
  mirror w = mirror <$> w


instance (ToJSON a) => ToJSON (Wheel a) where
  toJSON w =
    object
      [ "0"  .= wheel_0 w
      , "1"  .= wheel_1 w
      , "2"  .= wheel_2 w
      , "3"  .= wheel_3 w
      , "4"  .= wheel_4 w
      , "5"  .= wheel_5 w
      , "6"  .= wheel_6 w
      , "7"  .= wheel_7 w
      , "8"  .= wheel_8 w
      , "9"  .= wheel_9 w
      , "10" .= wheel_10 w
      , "11" .= wheel_11 w
      ]


instance (Semigroup a) => Semigroup (Wheel a) where
  a <> b = binary (<>) a b


instance (Monoid a, Semigroup a) => Monoid (Wheel a) where
  mappend = (<>)
  mempty = Wheel.init (const mempty)


instance Applicative Wheel where
  pure x = Wheel.init (const x)
  a <*> b = binary ($) a b


instance Foldable Wheel where
  foldMap f w =
    f (wheel_0 w)  `mappend`
    f (wheel_1 w)  `mappend`
    f (wheel_2 w)  `mappend`
    f (wheel_3 w)  `mappend`
    f (wheel_4 w)  `mappend`
    f (wheel_5 w)  `mappend`
    f (wheel_6 w)  `mappend`
    f (wheel_7 w)  `mappend`
    f (wheel_8 w)  `mappend`
    f (wheel_9 w)  `mappend`
    f (wheel_10 w) `mappend`
    f (wheel_11 w)


binary :: (a -> b -> c) -> Wheel a -> Wheel b -> Wheel c
binary f a b = Wheel
  { wheel_0  = f (wheel_0 a)  (wheel_0 b)
  , wheel_1  = f (wheel_1 a)  (wheel_1 b)
  , wheel_2  = f (wheel_2 a)  (wheel_2 b)
  , wheel_3  = f (wheel_3 a)  (wheel_3 b)
  , wheel_4  = f (wheel_4 a)  (wheel_4 b)
  , wheel_5  = f (wheel_5 a)  (wheel_5 b)
  , wheel_6  = f (wheel_6 a)  (wheel_6 b)
  , wheel_7  = f (wheel_7 a)  (wheel_7 b)
  , wheel_8  = f (wheel_8 a)  (wheel_8 b)
  , wheel_9  = f (wheel_9 a)  (wheel_9 b)
  , wheel_10 = f (wheel_10 a) (wheel_10 b)
  , wheel_11 = f (wheel_11 a) (wheel_11 b)
  }


fwrd :: Wheel a -> Wheel a
fwrd w = Wheel
  { wheel_0  = wheel_1 w
  , wheel_1  = wheel_2 w
  , wheel_2  = wheel_3 w
  , wheel_3  = wheel_4 w
  , wheel_4  = wheel_5 w
  , wheel_5  = wheel_6 w
  , wheel_6  = wheel_7 w
  , wheel_7  = wheel_8 w
  , wheel_8  = wheel_9 w
  , wheel_9  = wheel_10 w
  , wheel_10 = wheel_11 w
  , wheel_11 = wheel_0 w
  }


back :: Wheel a -> Wheel a
back w = Wheel
  { wheel_0  = wheel_11 w
  , wheel_1  = wheel_10 w
  , wheel_2  = wheel_9 w
  , wheel_3  = wheel_8 w
  , wheel_4  = wheel_7 w
  , wheel_5  = wheel_6 w
  , wheel_6  = wheel_5 w
  , wheel_7  = wheel_4 w
  , wheel_8  = wheel_3 w
  , wheel_9  = wheel_2 w
  , wheel_10 = wheel_1 w
  , wheel_11 = wheel_0 w
  }


init :: (Int -> a) -> Wheel a
init f = f <$> indexWheel


indexWheel :: Wheel Int
indexWheel = Wheel
  { wheel_0  = 0
  , wheel_1  = 1
  , wheel_2  = 2
  , wheel_3  = 3
  , wheel_4  = 4
  , wheel_5  = 5
  , wheel_6  = 6
  , wheel_7  = 7
  , wheel_8  = 8
  , wheel_9  = 9
  , wheel_10 = 10
  , wheel_11 = 11
  }


indexedMap :: (Int -> a -> b) -> Wheel a -> Wheel b
indexedMap f wheel =
  f <$> indexWheel <*> wheel
