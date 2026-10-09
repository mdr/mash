# Mash → Rust: port plan

Decisions agreed so far for porting Mash (Scala 2.12, ~37k lines) to Rust. Status as of 2026-10-09:
planning and design spikes complete, no production code yet. Spikes live locally in `rust/spikes/`
(not committed); each has a `FINDINGS.md`.

## Guiding principles

- **Full rewrite in Rust**, layer by layer. No JVM/Rust hybrid. The Scala implementation stays runnable as
  the reference for checking behaviour.
- **Idiomatic, not faithful.** Rust redesigns are welcome where they achieve the same features and
  behaviour; internals need not match Scala.
- **No language changes for now.** Small, deliberate behaviour changes are listed explicitly below; anything
  that changes the language (new syntax, new semantics) goes on the post-port backlog.
- **Tests are duplicated in Rust**, not extracted into a shared language-neutral corpus. The ~1,231
  `"expr" ==> "expected"` golden cases become a Rust test macro.
- **The core language must also run in the browser** (WASM). Core crates never touch the OS or terminal
  directly.
- **Code lives in this repo** under `rust/` (a Cargo workspace). The Nix flake provides the Rust toolchain
  (via `rust-overlay`), including the `wasm32-unknown-unknown` target.

## Crate layout

| Crate | Contents | WASM |
|---|---|---|
| `mash-syntax` | Lexer, rowan parser, typed syntax-tree accessors | yes |
| `mash-core` | Lowering + desugaring, lowered AST, values, evaluator, type inference, completion, pure builtins, `Platform` trait (fs, env, clock, processes) | yes |
| `mash-os` | Native-only builtins: os, users, processes, git, http, dns | no |
| `mash-repl` | Editor state machine, renderer to `Screen`, object browser. Key events in, `Screen` out; no terminal dependency | yes |
| `mash-cli` | crossterm backend, signals, process groups, `main` | no |
| `mash-web` (later) | wasm-bindgen glue, xterm.js backend | wasm only |

CI builds `mash-syntax`, `mash-core` and `mash-repl` for `wasm32-unknown-unknown` from day one, so OS
access can't creep into the core.

## Runtime and memory

**Values**
- `Value` enum: Null, Unit, Bool, Number, Str, List, Object, Function (closure), Builtin, BoundMethod,
  Class.
- `BoundMethod { target, method }`, where method is `Builtin(&'static …)` or `User(Gc<…>)`.
- `ClassRef` is `Builtin(&'static BuiltinClass)` or `User(Gc<Class>)`.
- Scopes are objects with a parent chain, as in Scala (leaky scopes for lambdas/blocks, full scopes for
  `def`s and methods).
- Containers never hand out borrows across calls back into Mash: iterate over a snapshot. This avoids
  `RefCell` re-entrancy panics.

**Garbage collection (no leaks)**
- Closures capture scope chains and scopes are objects, so cycles are routine; plain `Rc` would leak.
- **End state:** CPython-style design. `Rc` frees acyclic garbage immediately; a generational
  trial-deletion cycle detector over registered containers finds dead cycles. No root tracking is needed
  (references held in Rust locals count as external), so it's safe to collect mid-evaluation. Worklist, not
  recursion; iterative ("trashcan") drop for long chains.
- **Until then:** `dumpster` (unsync) behind the `Value` abstraction through M1–M3, replaced before M4.
- **Collection policy is ours:** always collect when the REPL goes idle, with heap doubling as a backstop
  during long commands. (dumpster's built-in trigger never fired for the closure-in-a-loop pattern.)
- The spike's leak tests become the collector's acceptance tests.

**Hash keys**
- Structural equality and hashing for strings, numbers, lists, objects; identity for functions and classes.
- Mutable container keys are **deep-copied on insert**, so mutating the original can't break lookup.

**Concurrency**
- Single-threaded runtime. `parallelMap` becomes a sequential `map`; `inBackground` runs synchronously or is
  dropped.
- Keep a later isolate-based design possible (deep-copy a closure + inputs into a separate interpreter on a
  worker thread): no thread-local globals in the value model.
- Concurrency lives inside builtins (on Rust data) and in external processes.

**Stack**
- The whole interpreter (REPL, evaluator, values) runs on one big-stack thread (~1 GB reserved, committed
  lazily). Signal handlers just set atomic flags.
- Depth limit counted in Mash calls, with a clean error: 10,000 natively; on wasm a ~16 MB link-time stack
  (~4,000 calls).
- Parser nesting limit ~1,000, which also bounds lowering, desugaring and inference.
- A CI test checks that recursion at the limit, in a debug build, fits the configured stack.
- Shrink evaluator frames only if needed. No explicit-stack evaluator.

**Interrupts**
- Ctrl-C sets an `AtomicBool` checked by the evaluator. In the browser, a step budget instead.

## Syntax and evaluation

**Parser (`mash-syntax`)**
- Hand-written lexer with a mode stack (normal, string interpolation, mish); a separate full pass.
- Hand-written recursive-descent parser emitting a **rowan** lossless syntax tree (rust-analyzer style).
  Errors become `ERROR` nodes and missing children; no synthetic tokens. Every input round-trips exactly.
- **No backtracking:** ambiguities (lambda start, pattern assignment, block vs object literal) are decided by
  bracket-balanced token scans. Any future grammar addition must be decidable the same way; otherwise switch
  to an event-based parser (rust-analyzer's Marker design).
- Thin typed accessor layer over the tree; may later be generated from a grammar description.

**Lowering and evaluation (`mash-core`)**
- Lower from the typed accessors to an immutable `Rc<Expr>` tree with a `NodeId` and source range per node
  (`TextRange` / `SyntaxNodePtr` instead of Scala's back-pointers).
- Desugar (headless members, holes, pipes) by **rebuilding `Rc` trees**, sharing untouched subtrees and
  preserving node IDs.
- Closures hold `Rc<FnDef>` directly; a single `unsafe impl Trace for FnDef` that visits nothing.
  **Invariant:** the lowered tree never contains `Value`s (constants are plain Rust data).
- User-function signatures are computed once at lowering.
- Tree-walking evaluator: `eval(&mut Ctx, &Expr, &Scope) -> Result<Value, MashError>`. `MashError` carries a
  message, source range and Mash-level stack trace (one frame added per call site). No bytecode or slot
  resolution.

**Type inference and IDE features**
- Inference returns a **side table** keyed by `NodeId` (types, pre-invocation types, bindings, constants).
  No mutable annotation slots on the tree; constant folding lives in the side table.
- Completion: find the relevant node in the rowan tree, map its range to a `NodeId`, read the side table.
  (Validated in the spike for globals, vectorised members, lambda params, pipeline results, user functions.)

## Language semantics

**Strings**
- One character = one Unicode scalar (`char`) for length, indexing, `indexOf`, and strings as sequences in
  collection functions. No normalisation. A `.graphemes` method may come later.
- Stored as `Rc<str>` plus tag, with a cached is-ASCII flag for O(1) indexing in the common case.
- Separate newtypes for byte offsets (internal), character indexes (visible to Mash) and display columns
  (terminal).
- Case mapping is locale-independent.
- Invalid UTF-8 from the OS is decoded lossily for now; paths may later carry the original `OsString`.

**Regex**
- `fancy-regex` (lookaround and backreferences; delegates to `regex` otherwise).
- Rust replacement syntax (`${1}`) and Rust `split` edge-case behaviour, pinned by tests.
- Unicode-aware case-insensitivity.
- Cache compiled regexes (regex values are just Regex-tagged strings).

**Numbers**
- Single `f64` behind an opaque `Number` type (`as_f64` / `as_i64`), so an integer/float split can be added
  later. Revisit when porting JSON (large IDs).
- Rust `Display` formatting; infinity printed as `Infinity`.
- `parseNumber`: trim, then standard parsing; accepts `Infinity` / `-Infinity` / `NaN`; Java's `1d` / `1f`
  suffixes dropped.
- Keys: `-0.0` hashes as `0.0`; NaN equals itself for key purposes only (ordinary `==` unchanged).
- Integer arguments via `as_i64` and checked `usize` conversion; no 32-bit limit.
- Sorting uses `f64::total_cmp`.
- JSON: whole numbers written as integers; writing NaN or Infinity is an error.

**Dates and times** (`jiff`)
- DateTime = `Timestamp` read in the system zone; Date = `civil::Date`. No new Date methods for now.
- Clock and time zone come from the `Platform` trait (fixed clock in tests). Never call `Timestamp::now()`
  directly.
- Durations stay tagged numbers; `dt1 - dt2` is a Milliseconds-tagged number.
- `dt ± n.unit` uses the same zoned calendar arithmetic as `ago` / `fromNow` (fixes `now + 1.months`
  throwing and the DST mismatch). Fractions exact for milliseconds–hours; an error for days/weeks/months.
- `toDateTime`: strict jiff formats (RFC 3339/ISO, RFC 2822, Mash's own `format` output) → ~80-line
  normaliser → uutils `parse_datetime`. `null` on failure. Fuzz-tested never to panic. (`interim` rejected.)
- `parseIso8601` via jiff.
- `format`: fixed English, locale-independent, zone abbreviation only, e.g. `Fri 9 Oct 2026 10:00:00 BST`.
  Round-trips in the local zone or GMT/UTC; ISO 8601 is the interchange format.
- Table display: ISO-style local time (`2026-03-28 12:00:00`); fuzzy display optional.
- `fuzzy`: hand-written; largest whole unit, rounded down ("moments ago", "3 days ago", "2 hours from now").

## Builtins

- **Start with plain traits** (`BuiltinFunction` / `BuiltinMethod`) with a `Signature` builder and typed
  extraction via `FromValue` (`args.get::<T>("name")?`). Builtins take `ctx: &mut Ctx` to call back into Mash.
- Builtins are `'static` (avoids borrow conflicts with `Ctx`).
- Type inference per builtin: `Inference::Returns(Type)` or `Inference::Custom(strategy)`.
- Optional completion function; help stored as data, keeping the `<mash>` markup.
- Each namespace exposes `register(&mut Registry)`; native-only namespaces live in `mash-os`.
- Collection functions are also exposed as List/String methods via a generic wrapper ("methodise").
- Never call fallible Mash functions inside std callbacks (e.g. `sort_by`'s comparator): precompute keys.
- **Attribute macro** (`#[builtin(...)]`, `#[builtin_method(...)]`) arrives after the first ~10 builtins in
  M2, before the M3 bulk port. It generates the same `Signature` / `FromValue` code. Help from the doc
  comment; parameter summaries in the attribute; shared parameters via constants (`def = DESCENDING`);
  `args: &Args` escape hatch for lazy / named-args / all-args builtins. `FromValue` for `Vec<T>`,
  `Option<T>` and domain types. A registry test validates every signature.

## Standard library

| Area | Decision |
|---|---|
| git | Shell out to `git`, parsing machine formats (`status --porcelain=v2 --branch`, `log --format` with NULs, `for-each-ref`) into the same structured values. Small internal git interface so hot read paths can move to `gix` later. New integration tests using temp repos. |
| http | `reqwest`/`ureq` with rustls. **TLS verified by default** (today trust-all is unconditional and installed JVM-wide); opt-in `--insecure`. |
| xml | Reimplement org.json's shape rules on `quick-xml`, but keep text as strings (no number/boolean coercion). |
| users/groups | `getpwent` / `getgrent` / `getpwuid` (via `nix` or `uzers`) on both Linux and macOS. |
| glob | Per-segment `globset`; `*` skips dotfiles unless the pattern starts with `.`; follow symlinks with loop detection (`walkdir`). |
| other deps | oshi → `sysinfo`; jnr-posix → `std`/`nix`; gson → `serde_json` (`preserve_order`); commons-csv → `csv`; prettytime → hand-written; commons-math → hand-rolled; natural-comparator → `natord`; lanterna and ant dropped (unused). |

## Subprocesses

During the port (no language change):
- Write stdin and read stdout concurrently (fixes today's deadlock risk).
- Each child in its own process group, given the terminal with `tcsetpgrp`; Mash ignores `SIGTTOU`. A child
  killed by SIGINT aborts evaluation cleanly.
- Direct termios (no spawning `stty`); `cd` via `std::env::set_current_dir`.
- Ctrl-Z before job control exists: resume the child immediately and print a message.
- Windows out of scope.

## REPL and line editor

- **Custom editor**, not reedline/rustyline (they own terminal I/O, which rules out the browser).
- Input: crossterm events → Mash `KeyEvent` natively; xterm.js events in the browser. Fixes today's UTF-8
  input, bare Esc and resize bugs; adds bracketed paste.
- Cursor movement and deletion by grapheme; widths via `unicode-width`.
- Port `Screen` / `StyledString` / the diffing renderer, with a backend trait (crossterm, or an ANSI string
  for xterm.js).
- Multi-line input accepted when the buffer parses completely.
- History: keep `~/.mash/history` (JSON lines) compatible; load only the tail; append under a file lock.
- Object browser: port Mash's own renderers (not ratatui) initially.

## Milestones

| # | Milestone | Contents |
|---|---|---|
| M0 | Workspace | Real crates as above; CI incl. wasm build check |
| M1 | Syntax | Lexer, rowan parser, typed accessors (largely from the parser spike); port lexer/parser tests |
| M2 | Evaluator | Value model, lowering, desugaring, evaluator, core + collections builtins used by the evaluator tests; `mash -c` and `mash script.mash`; builtin macro after ~10 builtins; port the golden evaluator tests |
| M3 | Pure stdlib + IDE logic | String/object/number methods, json, time, maths, hash, etc.; type inference; completion and assistance (tested without a terminal) |
| — | GC swap | Replace dumpster with the CPython-style collector before M4 |
| M4 | Interactive shell | Line editor, screen, keys, history, completion UI, `!{…}`, `cd`, minimal os (ls, read, write, run) — daily-usable |
| M5 | Remaining OS | Rest of os, users/groups, http, git, object browser |
| M6 | Browser | `mash-web` with xterm.js |

## Post-port backlog (language changes, deferred)

- Pipes between external commands inside `!{…}`; `2>` / `2>&1` and stderr capture.
- Background jobs (`&`), `jobs` / `fg` / `bg`, a stopped-job list for Ctrl-Z.
- Streaming process output (needs lazy sequences).
- Integer/float number split.
- Isolate-based `parallelMap` / `inBackground`.
- `.graphemes`, Date methods, paths carrying `OsString`, localised date formatting.

## Spike results (summary)

| Spike | Result |
|---|---|
| `spikes/values` | dumpster frees all cyclic shapes and runs on wasm, but needs our own collection policy; collection pauses grow with the heap and recurse; dropping a handle can scan a whole value. → CPython-style collector as end state. |
| `spikes/parser` | Full grammar on rowan, 164 tests, lossless on every prefix, no backtracking; 10k lines in 8.5 ms, a REPL line in 4 µs; 30k-deep nesting overflows → nesting limit. |
| `spikes/slice` | End-to-end mini-Mash: 125 ported golden cases + 14 integration tests; completion from the inference side table works; GC bounded under real evaluation; ~3.5 KB stack per call (release) → big-stack thread; needed `BoundMethod`. |
| `spikes/dates` | Baseline natty run: 45/54 correct but returns garbage on partial matches. Proposed pipeline 46/54 with 1 wrong; `interim` panics on our `format` output. |
