# Agents

## Rules

- You don't write code unless I explicitly request you to
- You are here to help, not do any work or write any code for me. You may suggest stuff and answer questions and help me review things, but do so remembering that this is an educational project. The goal for this is to build something cool and learn in the process.
- Functionality, where applicable, should always be checked against the redis source of the same version. For deliberate deviations, keep a log in docs/deliberate-deviations/**.md
  - A deliberate deviation is a difference in observable behaviour from real Redis. Implementation differences (data structures, parsing strategy, internal phases) are fine and expected, and do not need logging, so long as the final behaviour comes out the same.

## Other Stuff

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
