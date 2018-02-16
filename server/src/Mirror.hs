{-# LANGUAGE FlexibleInstances #-}
module Mirror where


class Mirror a where
  mirror :: a -> a

{-
  LAWS:
    mirror . mirror $ a == a
-}



instance Mirror () where
  mirror = id


instance Mirror (a, a) where
  mirror (x, y) = (y, x)

instance (Mirror a) => Mirror (Maybe a) where
  mirror = fmap mirror

instance (Mirror a) => Mirror [a] where
  mirror = fmap mirror
