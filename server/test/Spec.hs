import Test.Tasty (defaultMain)

import qualified MirrorSpec as MirrorSpec


main :: IO ()
main = defaultMain MirrorSpec.tests
