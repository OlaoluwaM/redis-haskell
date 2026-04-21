# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A Redis server implementation in Haskell, built as a [CodeCrafters](https://codecrafters.io) challenge. It supports PING, ECHO, SET, GET, CONFIG GET, SAVE, BGSAVE, LASTSAVE, and KEYS commands, with RDB file persistence (load and save).

## Build & Run

```bash
# Build (uses Stack with LTS-23.28 / GHC 9.8)
stack build --fast --pedantic

# Run the server (builds then runs)
./your_program.sh

# Run with options
./your_program.sh --port 6380 --dir /tmp --dbfilename dump.rdb
```

## Testing

```bash
# Run all tests
stack test

# Run a single test module (tasty-discover pattern)
stack test --test-arguments '-p "/SetSpec/"'

# Run tests matching a specific name
stack test --test-arguments '-p "some test name"'
```

Tests use `tasty-discover` for auto-discovery (see `test/Spec.hs`). Test files follow the `*Spec.hs` naming convention and live under `codecrafters-redis/test/`, mirroring the source tree.

## Formatting & Linting

Fourmolu and HLint are installed via isolated Stack projects under `tools/`:

```bash
cd tools && make fourmolu   # install fourmolu
cd tools && make hlint      # install hlint
```

## Architecture

### Two Stack packages

- **codecrafters-redis** — the main server: library (`src/`), executable (`app/`), tests (`test/`), benchmarks (`bench/`)
- **codecrafters-redis-ffi** — C FFI bindings for CRC64 (checksum) and LZF (compression), used by the RDB binary codec

### Effect system (effectful)

The server uses the `effectful` library for managing side effects. Effects are defined in `Redis.Effect.*` and composed in `Redis.Effects`:

- `Communication` — socket send (has an IO interpreter for production and a pure `Writer`-based interpreter for testing)
- `Time` — current time / POSIX time
- `Logging` — structured logging via Blammo

`ServerEffects` is the canonical effect stack: `Reader r, FileSystem, Concurrent, Time, Logging, Communication, IOE`.

Constraint aliases in `Redis.Effects` (`RedisClientCommunication`, `RedisServerState`, `RedisServerSettings`, `RDBWrite`) abstract over required capabilities so command handlers stay polymorphic in the environment type `r`.

### Request lifecycle

1. `app/Main.hs` — TCP accept loop, reads raw bytes from socket
2. `Redis.Handler.handleCommandReq` — parses RESP bytes via `Redis.Commands.Parser.commandParser` (attoparsec), dispatches to per-command handler
3. Command handlers (e.g. `Redis.Commands.Set`) read/write `ServerState` via STM and send RESP responses through the `Communication` effect

### Server context & state

- `ServerContext` (`Redis.Server.Context`) — per-connection: holds the client `Socket`, shared `ServerState`, and `ServerSettingsRef`
- `ServerState` (`Redis.ServerState`) — shared across connections: `TVar Store` (key-value HashMap) and `TVar LastRDBSave`
- `ServerSettings` (`Redis.Server.Settings`) — runtime-mutable settings stored as `TVar (HashMap Setting SettingValue)`, parsed from CLI via `optparse-applicative`

Field labels are generated with `makeFieldLabelsNoPrefix` (optics-th), and the codebase accesses them with `OverloadedLabels` / `OverloadedRecordDot`.

### RESP protocol

`Redis.RESP` implements the Redis Serialization Protocol: parsing (`attoparsec`) and serialization of SimpleString, BulkString, Array, Integer, and Null types.

### RDB persistence

`Redis.RDB.*` handles the RDB binary format:
- `Binary` — custom `RDBBinary` typeclass (not `Data.Binary`) for encode/decode, with `RDBConfig` controlling compression and checksum behavior
- `Data` / `Format` — RDB file structure types (magic string, version, aux fields, DB entries, key-value opcodes)
- `Load` / `Save` — convert between in-memory `Store` and `RDBFile`

### Testing approach

Tests run the effect stack with `runTestServer` (`Redis.Test`), which swaps `Communication` for a pure writer (`runCommunicationPure`) so tests capture responses without network I/O. The test framework is tasty with hspec, hedgehog (property tests), and golden tests.

## Language & Extension Notes

- Language standard: `GHC2021`
- `StrictData` is enabled globally — all data fields are strict by default
- `NoFieldSelectors` + `OverloadedRecordDot` — field access uses dot syntax, not selector functions
- `-Wmissing-export-lists` is on — every module must have an explicit export list
