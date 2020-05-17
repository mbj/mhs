![CI](https://github.com/mbj/tasty-mgolden/workflows/CI/badge.svg)

# tasty-mgolden

Text based [golden tests](https://ro-che.info/articles/2017-12-04-golden-tests)
for [tasty](https://github.com/feuerbach/tasty), with multi line (colored) diff expectation
failure reporting.

Basically this package can turn any `IO Text` action into a golden test via providing a
`FilePath`.

This package is the small brother of [tasty-golden](https://github.com/feuerbach/tasty-golden)
which also implements the same golden testing pattern.

`tasty-mgolden` is less generic and more opinionated than its bigger brother.
But for that reason can also offer more ergonomic defaults for its core use case:

Golden testing for `IO Text` actions. Where:

* The text value is assuemd to contain multiple lines.
* The expectation failures can be rendered nicely in multi line diffs.
* The diff lines are colored via the tasty infrastructure
* The diff rendering is not done via the external `diff` command, but uses the `Diff` package.

Expectation diffs are rendered like developers are used to:

* Removed lines rendered in red.
* Added lines in green.

## Usage

This is taken from the [example](example) directory in the repository.

To run these execute from repository root:

```
stack build
stack exec tasty-mgolden-example
```

Contents of [example/example-a.txt](example/example-a.txt)

```
foo
bar
```

Contents of [example/example-b.txt](example/example-b.txt)

```
foo
bar
```

Contents: `example/Test.hs`

```haskell
import Test.Tasty
import Test.Tasty.MGolden

main :: IO ()
main = defaultMain $ testGroup "golden tests"
  [ goldenTest "example-a" "example/example-a.txt" $ pure "foo\nbar\n"
  , goldenTest "example-b" "example/example-b.txt" $ pure "foo\nbaz\n"
  ]
```

Output:

```diff
golden tests
  example-a: OK
  example-b: FAIL
 foo
-bar
+baz

1 out of 2 tests failed (0.00s)
```

Passing the `--update` flag to accept the changes:

```
golden tests
  example-a: OK
  example-b: OK
    UPDATE

All 2 tests passed (0.00s)
```

### TODOs

PRs on  these are welcome.

* Improve the multi line diff reporting to only show a
  minimal context around the changed hunks.
* Add line markers in unified diff format.
* Change to `pathtype` from `filepath`
* Future? Upstream this to `tasty-golden` ?
