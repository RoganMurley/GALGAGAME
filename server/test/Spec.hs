import Test.Tasty (defaultMain, testGroup)

import qualified MirrorSpec
import qualified StackSpec


main :: IO ()
main = defaultMain $
  testGroup "Tests"
    [ MirrorSpec.tests
    , StackSpec.tests
    ]
