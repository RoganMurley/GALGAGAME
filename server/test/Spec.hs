import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Mirror
import Player (WhichPlayer(..))


instance Arbitrary WhichPlayer where
  arbitrary = elements [PlayerA, PlayerB]


main :: IO ()
main = defaultMain $
  testGroup "Mirror"
    [
      whichPlayer
    ]


mirrorOnceProperty :: (Eq a, Mirror a) => a -> Bool
mirrorOnceProperty x = mirror x /= x


mirrorTwiceProperty :: (Eq a, Mirror a) => a -> Bool
mirrorTwiceProperty x = (mirror . mirror $ x) == x


whichPlayer :: TestTree
whichPlayer = testGroup "WhichPlayer"
  [ QC.testProperty "once"  (mirrorOnceProperty  :: WhichPlayer -> Bool)
  , QC.testProperty "twice" (mirrorTwiceProperty :: WhichPlayer -> Bool)
  ]
