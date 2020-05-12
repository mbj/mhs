![CI](https://github.com/mbj/source-constraints/workflows/CI/badge.svg)

# source-constraints

An in progress GHC plugin to add constraints on the GHC AST.

## Installation

Add the following extra-dep to your `stack.yml`:

```yaml
extra-deps:
- git: ssh://git@github.com/mbj/source-constraints
  commit: $use_current_commit_hash
```

Add the plugin to your `packages.yml`

```yaml
ghc-options:
- -fplugin=SourceConstraints
```
