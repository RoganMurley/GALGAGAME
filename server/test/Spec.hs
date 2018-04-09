import Test.Tasty (defaultMain, testGroup)

import qualified MirrorSpec


main :: IO ()
main = defaultMain $
  testGroup "Tests"
    [ MirrorSpec.tests
    ]
