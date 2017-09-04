{-# LANGUAGE FlexibleInstances #-}
module Mirror where


class Mirror a where
  mirror :: a -> a

{-
  LAWS:
    mirror . mirror $ a == a
-}


instance Mirror (a, a) where
  mirror (x, y) = (y, x)
