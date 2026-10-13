# AGENTS.md

## Rules

- When asked to write tests: if the tests reveal failures in existing code, report them back and stop there. Do not attempt to fix the underlying implementation unless explicitly asked to as a separate step.

## Test-only exports (`x-unsafe-internals`)

Library exports that exist only so tests can reach them are marked with a custom warning category:

```haskell
{-# WARNING in "x-unsafe-internals" someInternal "This value is exported for testing purposes only" #-}
```

Any non-test module that uses such an export gets a warning (an error under `-Werror`), which is the point: the category acts as a compiler-enforced "tests only" fence.

Every spec module that uses one of these exports MUST suppress the category via a module-level pragma, not (only) via the test stanza's `ghc-options`:

```haskell
{-# OPTIONS_GHC -Wno-x-unsafe-internals #-}
```

Why the pragma is required: `stack build --pedantic` appends `-Wall -Werror` *after* the stanza's `ghc-options`, GHC processes warning flags left to right, and `-Wall` re-enables custom warning categories — silently undoing an earlier stanza-level `-Wno-x-unsafe-internals`, so `-Werror` then fails the build. `OPTIONS_GHC` pragmas are processed after the entire command line, so they win regardless of flag ordering. (Verified against GHC 9.8.4: `-Wno-x-foo -Werror` stays clean, `-Wno-x-foo -Wall -Werror` errors, `-Wall -Werror -Wno-x-foo` stays clean.)

The stanza-level `-Wno-x-unsafe-internals` in `package.yaml` is kept for ordinary builds, but do not rely on it alone.
