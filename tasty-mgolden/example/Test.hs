import Test.Tasty
import Test.Tasty.MGolden

main :: IO ()
main = defaultMain $ testGroup "golden tests"
  [ goldenTest "example-a" "example/example-a.txt" $ pure "foo\nbar\n"
  , goldenTest "example-b" "example/example-b.txt" $ pure "foo\nbaz\n"
  ]
