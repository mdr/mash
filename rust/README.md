# Mash in Rust

A Rust port of Mash, in progress. See [PORT_PLAN.md](PORT_PLAN.md) for the design decisions and milestones.

| Crate | Contents |
|---|---|
| `mash-syntax` | Lexer and lossless `rowan` parser |
| `mash-core` | Values, evaluator, type inference, pure builtins, `Platform` trait (builds for wasm) |
| `mash-os` | Native-only builtins and the native `Platform` |
| `mash-repl` | Terminal-independent REPL (builds for wasm) |
| `mash-cli` | The `mash` binary |

The toolchain comes from the Nix flake at the repo root, including the `wasm32-unknown-unknown` target:

    nix develop
    cd rust
    cargo test --workspace
    cargo build --target wasm32-unknown-unknown -p mash-syntax -p mash-core -p mash-repl
