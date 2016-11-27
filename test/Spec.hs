import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
  testGroup "Tests"
    [ testGroup "Unit tests"
      [ testCase "List comparison (different length)" $
        [1, 2, 3] `compare` [1,2] @?= GT

      , testCase "List comparison (same length)" $
        [1, 2, 3] `compare` [1,2,2] @?= EQ
      ]
    ]
