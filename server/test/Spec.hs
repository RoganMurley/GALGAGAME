import qualified MirrorSpec
import qualified StackSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ MirrorSpec.tests,
        StackSpec.tests
      ]
