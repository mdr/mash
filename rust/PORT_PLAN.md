# Mash → Rust: port plan

Decisions agreed so far for porting Mash (Scala 2.12, ~37k lines) to Rust. Status as of 2026-10-09: M0–M6
done (syntax, values, evaluator, the pure standard library, type inference, completion and invocation assistance,
`mash -e`/script runner, the GC swap, the interactive shell with the file system and external commands, the
rest: users/groups, processes, git, http, the object browser; and the browser build). The native code is
tested on macOS and Linux (`spikes/linux/run.sh`: the test suite plus manual checks in a Debian container). Spikes live
locally in `rust/spikes/` (not committed); each has a `FINDINGS.md`. `rust/TESTS.md` tracks which Scala tests are
ported.

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
| `mash-web` | The browser build: wasm-bindgen glue, the browser's `Platform`, the REPL host, and the page (`www/`: xterm.js, a Web Worker) | wasm (its platform, host and key translation also build natively, for tests) |

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
- **Our own collector** (`mash-core/src/value/gc.rs`), CPython-style. Reference counts free acyclic garbage
  immediately; a two-generation trial-deletion cycle detector over all heap boxes finds dead cycles. No root
  tracking is needed (references held in Rust count as external), so it's safe to collect mid-evaluation; a
  container whose `RefCell` is mutably borrowed is treated as reachable. Worklist, not recursion; freeing is
  iterative, so million-deep chains drop without overflowing the stack.
- **Policy:** a young collection when 10k young boxes are alive (it never traces the old heap); a full one
  instead when the old generation has doubled since the last. The REPL should call `gc::collect()` when idle.
- Replaced `dumpster` (used through M1–M3) after M3. Same benchmark, release: clone+drop 15.8 → 3.2 ns;
  reclaiming 1M garbage cycles 678 → 86 ms; closure loop with 1M objects in globals 924 → 231 ns/call, worst
  pause 643 → 0.9 ms; `fib 25` 129 → 108 ms. The cost: a full collection traverses the whole heap even when
  it's clean (35 ms for 1M live objects, vs ~0 for dumpster). `examples/gc_bench.rs` reproduces these.
- The spike's leak tests are the collector's acceptance tests (`tests/value_leaks.rs`).

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
- Depth limit counted in Mash calls, with a clean error: 10,000. In the browser the stack is the engine's
  (see M6 design): before each call Mash asks the platform whether the stack is nearly full, and stops with
  the same error.
- Parser nesting limit ~1,000, which also bounds lowering, desugaring and inference.
- A test checks that recursion at the limit fits the stack natively; in the browser, `mash-web`'s tests check
  that deep recursion stops cleanly (`www/test/wasm.mjs` in Node, `www/test/browser.mjs` in real browsers).
- Shrink evaluator frames only if needed. No explicit-stack evaluator.

**Interrupts**
- Ctrl-C sets an `AtomicBool` checked by the evaluator. In the browser, Ctrl-C sets a flag in memory shared with
  Mash's worker, which the evaluator polls every 1,024 steps through `Platform::interrupt_requested` (instead of
  the step budget first planned, which would have stopped long computations the user wanted). `Ctx::step_budget`
  remains, for embedders.

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
| http | `ureq` 3 with rustls (ring) and the system's trust store (`rustls-platform-verifier`). **TLS verified by default** (today trust-all is unconditional and installed JVM-wide); opt-in `--insecure`. Redirects and cookies handled in `mash-os` as Apache's client did. |
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

**M4 design (decided while starting M4):**
- `Platform` file and process API (all with defaults that fail as unsupported): `set_current_dir`, `metadata` /
  `symlink_metadata` (kind, size, permission bits, owner/group names, modified/accessed), `read_link`,
  `canonicalize`, `read_file`, `write_file` (append), `create_dir` (intermediates), `remove` (recursive, never
  follows links), `copy_file`, `rename`, `set_permissions`, `create_temp_file` / `create_temp_dir`, and
  `run_process(ProcessRequest { argv, env, stdin: Inherit | Text | File, stdout: Inherit | Capture | File })` →
  `ProcessOutcome { exit_status, stdout, started, finished, interrupted }`. Directory copies, cross-device moves,
  globbing and recursive listing are built on these in `mash-core`. `TestPlatform` has `MemoryFiles` (contents,
  permissions, links) and scriptable fake commands (`echo`, `cat`, `true`, `false`; add with `with_command`).
- The child's environment is the `env` global's contents (as in Scala), not the host's.
- The interpreter keeps the working-directory history (`Ctx::working_directories`); `cd` changes the host's
  current directory.
- `mash-repl` modules: `screen` (styled text, `Screen`), `input` (`KeyPress`/`InputEvent`, translated by the
  host), `host` (the `Host` trait: size, output, and switching terminal modes around commands), `history`
  (entries and a `HistoryStorage` trait), `view` (printing command results), plus the editor/REPL itself.
- **OS builtins live in `mash-core`, over the `Platform` trait** (files, directories, processes), not in
  `mash-os`. They are then testable with `TestPlatform`'s in-memory files, and could work in the browser over
  a virtual file system. `mash-os` provides `NativePlatform` (and later the native-only namespaces).
- Commands run with the terminal in its normal (cooked) mode; the editor uses raw mode only while editing.
  Ctrl-C during a command is SIGINT, which sets the interpreter's interrupt flag.
- Output too large for the screen opens the object browser (M5; see below).

**M5 design:**
- **git runs the `git` command** (porcelain/plumbing output formats, through `Platform::run_process`) rather than
  a library: everything works as the user's own git does (credentials, ssh agent, config, hooks), including
  push/pull/fetch, which `gix` lacks; no C dependency (`git2`). Requires `git` on the PATH.
  Done: `mash-core/src/builtins/git/` — `run.rs` finds the repository (walking up for `.git` through the
  `Platform`, so nothing runs outside one) and runs git in three modes (read-only queries: no stdin, `LC_ALL=C`,
  `GIT_OPTIONAL_LOCKS=0`; quiet changes: stdout captured, stdin the terminal for hooks and credentials; changes
  whose output the user sees: merge, pull); `parse.rs` parses `status --porcelain=v2 -z`, `log -z --format`,
  `for-each-ref --format`, `diff-tree -z`, `reflog -z`, `push --porcelain`; fetch/pull results compare the
  remote-tracking refs before and after. Tag classes' members (`git.CommitHash`, `LocalBranchName`,
  `RemoteBranchName`) look up the object (`lifter.rs`, Scala `MemberLifter`). Completions that need to run git
  use `CompletionSpec::Computed(fn(&mut Ctx) -> Vec<CompletionSpec>)`, evaluated only when completing.
- **http and dns go through the `Platform` too** (`ureq` with rustls natively, TLS verified by default,
  `--insecure` to opt out), so the builtins stay in `mash-core`, are testable with canned responses, and could
  use `fetch` in the browser. `Platform::http_request(&HttpRequest { method, url, headers, body, basic_auth,
  cookies, insecure }) -> io::Result<HttpResponse { status, headers, cookies, body }>` (any status is a response;
  errors carry user-facing messages; `HttpRequest::all_headers` turns cookies and basic auth into headers for
  every implementation), `lookup_host(host) -> io::Result<Vec<String>>`, and `open_with_default_application(target)`
  (`open`/`xdg-open`; also used by `os.open` and `Path.open`). `TestPlatform` answers canned responses by method
  and URL (`with_http_response`), records requests (`http_requests()`), resolves `hosts`, and records `opened`.
- **`mash.reload`** resets the standard globals in `mash-core`, then runs a hook the host installs on the `Ctx`
  (`Ctx::set_reload_hook`): the shell's (`mash-cli` `startup::reload_hook`) loads `~/.mash/lib` and runs
  `~/.mash/init.mash` again. Scripts and `mash -e` have no hook. `mash.source` runs a file in the global scope.
- Users, groups, processes, disk space and the clipboard are `Platform` methods as well; the REPL gives
  `mash-core` access to its history (for `history` and `jump`) through a small trait.
- **System `Platform` API** (defaults fail as unsupported; `terminal_size` defaults to None): `users()` / `groups()`
  (whole password/group databases: `UserEntry { name, uid, gid, gecos, home, shell }`, `GroupEntry { name, gid,
  members }`), `current_user()`, `processes()` / `process(pid)` (`ProcessInfo { pid, parent_pid, name, command,
  owner: Option, resident_size, virtual_size }`), `kill(pid, &Signal)` (`Signal::Named` from the POSIX
  `SIGNAL_NAMES`, or `Signal::Number`), `clipboard()` / `set_clipboard(text)`, `disk_space()` (`DiskSpace {
  filesystem, total, used, available, mount_point }`), `terminal_size()`. `TestPlatform::system` is a `FakeSystem`
  (users `root`/`alice`/`bob`, groups, five processes that signals remove, a clipboard, two disks, an 80×24
  terminal; records signals). `open` and `Path.open` use `open_with_default_application` (above). Natively
  (`mash-os`): `uzers` walks `getpwent`/`getgrent` (one `unsafe` call each, serialised by a mutex) on Linux and
  macOS; `sysinfo` lists processes; `nix` sends signals; `df -kP` gives disk space; `pbcopy`/`pbpaste` or
  `wl-copy`/`xclip`/`xsel`; crossterm reads the terminal size when one of the standard streams is a terminal.
- **History access:** `mash_core::history::{CommandHistory, HistoryEntry, RecordedHistory}`;
  `Ctx::set_command_history`. The REPL keeps a `RecordedHistory` (loaded entries with null results, then each
  command with its result) alongside its own `History`. Without a REPL, `history` is empty.
- Tag classes whose values come from the system (`Username`, `Group`, `Pid`) compute them for completion with
  `BuiltinClass::enumerate`; `CompletionSpec::Computed` completes arguments from the interpreter's state
  (`jump` from the history).
- **REPL API** (`mash-repl::repl`): `Repl::new(ctx, Box<dyn HistoryStorage>, session_id)`; the host calls
  `start(&mut host)` (startup tip, first prompt), then `handle_event(InputEvent, &mut host) -> ReplStatus` for
  each key press, paste or resize, until it returns `Exit` (Ctrl-D on an empty line). The REPL draws through
  `Host::write` (ANSI from `draw::ScreenDrawer`, which writes only what changed), and runs each command between
  `Host::begin_command` and `Host::end_command`. `handle_action`, `render` and `draw` are public for tests and
  other front ends; `run_script` runs an init file. Ctrl-V browses the last result.
- **Object browser** (`mash-repl::browser`, M5): a port of Scala's browser states, handlers and renderers. The
  browser's state is a stack of `BrowserState`s (one variant per kind of view: 2D table, object fields, tree,
  text lines, help, value) in `ReplState::object_browser`; its actions are `Action::Browser(BrowserAction)`,
  with key maps for the browser, its incremental search and its expression line. The printers in
  `mash-repl::view` were split into a model and a renderer, so the browser draws the same tables and trees,
  decorated with the cursor, marks and search hits. `view::view_or_browse` decides between printing and
  browsing (`config.view.browseLargeOutput`, `view.browse`, `view.print`), and help records its links. The
  expression line reuses the REPL's completion and assistance; expressions run with `it` bound to the value
  and the globals restored afterwards (Scala used a copy of them). The browser is drawn on the alternate
  screen, which `draw::ScreenDrawer` already supported.

**M6 design:**

- **Mash runs in a Web Worker**, so a long command doesn't freeze the page. The page (`www/main.js`) shows an
  xterm.js terminal and sends the worker `keydown` events, pastes and resizes; the worker (`www/worker.js`)
  hands them to `WebShell` (wasm-bindgen; `shell::Shell` is the REPL with the browser's platform) and sends back
  what to write. Key events are translated in Rust (`keys.rs`): Meta (Command) shortcuts stay with the browser,
  and with Alt on macOS (where Option changes the character) the physical key is used.
- **`Bridge`** (`bridge.rs`) is what Mash needs from the worker's JavaScript: writing to the terminal, the clock
  and time zone, randomness, interrupts and the stack check, sleeping, history, the clipboard, opening URLs and
  HTTP. Tests use `FakeBridge`.
- **`WebPlatform`**: files in memory (`MemoryFiles`, owned by the one user `guest`, home `/home/guest` with sample
  files, lost when the page closes); HTTP by synchronous `XMLHttpRequest` (allowed in workers; subject to CORS
  and forbidden headers; no cookies from responses); `open` for http(s) URLs only (a new tab); the clipboard
  can be written but not read (paste with the browser's own keys); the time zone from `Intl`, using jiff's
  bundled database. Running programs, processes, signals, disk space and DNS fail with "… is not possible in
  the browser". There are no `~/.mash` library or init files.
- **History** is kept in the page's local storage (the last 2,000 entries), in the same JSON format as
  `~/.mash/history` (`HistoryEntry::to_json`, now in `mash-repl`).
- **Ctrl-C** while a command runs: if the page is cross-origin isolated (`www/serve.py` sends the headers;
  where a host can't, as on GitHub Pages, `www/coi-serviceworker.js` adds them, over HTTPS only), it sets a flag in a `SharedArrayBuffer` that Mash polls, and `sleep` waits on it with `Atomics.wait`. Otherwise
  the page can't reach the busy worker, so it stops the worker and starts a new session, saying so.
- **The stack.** wasm runs on the browser's native stack (Chromium gives a worker ~460 KB; WebKit less) at a
  couple of hundred bytes per wasm frame, whatever the function; overflowing it throws through Mash's frames,
  leaving it unusable. So the evaluator keeps frames per Mash call few (`eval_expr` handles common expressions
  inline and loops on tail positions; a call is one frame), and before each call `Platform::stack_nearly_full`
  checks both the wasm stack (16 MB, set in `build.rs`) and the native stack, probed from JavaScript (`stack.js`:
  a call with 8,192 arguments needs 64 KB; if it doesn't fit, the RangeError is caught in JavaScript). It
  probes only on getting 8 KB (of wasm stack) deeper than at the last probe. Plain recursion stops at about 400
  levels in Chromium, 1,130 in Firefox and 115 in WebKit; through `map` and a lambda, 165, 360 and 40.
- **Shared sessions:** `share` (browser only, `share.rs`; `share 3` for the last three) makes a link to the page
  carrying this session's commands in the fragment, `#run=<command>&run=<command>…`, each percent-encoded, and
  copies it to the clipboard. Opening a link lists its commands and runs them only on Enter (Esc starts without
  them): the worker types each in turn (`WebShell.runCommand`), waiting while the object browser is open;
  Ctrl-C, or typing, stops the rest.
- **Panics and overflows** that do happen stop the worker; the page starts a new session and says why.
- **Building and testing:** `nix develop -c rust/crates/mash-web/build.sh [--dev]` (wasm-bindgen and wasm-opt
  from the flake) writes `www/pkg/`; `python3 www/serve.py 8000 www` serves the page. `www/test/wasm.mjs` checks
  the wasm in Node; `www/test/browser.mjs` (recursion) and `www/test/page.mjs` (typing commands, Ctrl-C with and
  without isolation, history across reloads, the object browser, Ctrl-D) run in headless Chromium, Firefox and
  WebKit via Playwright (not in the flake: put `playwright-core` on `NODE_PATH`; xterm.js comes from a CDN).
- **Publishing:** the `Publish web version` workflow (`.github/workflows/publish-web.yml`, run by hand: `gh
  workflow run publish-web.yml`) builds with Determinate Nix, checks the wasm in Node, collects the page with
  `site.sh` and commits it to `try/` on the `gh-pages` branch, next to the docs site. `MASH_WWW=dir
  www/test/page.mjs` tests a collected site.
- **Not done:** persistent files (e.g. IndexedDB), reading the clipboard, uploading or downloading files, and
  more depth in WebKit (fewer frames per builtin call, or eventually an evaluator with its own stack).

## Milestones

| # | Milestone | Contents |
|---|---|---|
| M0 | Workspace | Real crates as above; CI incl. wasm build check |
| M1 | Syntax | Lexer, rowan parser, typed accessors (largely from the parser spike); port lexer/parser tests |
| M2 | Evaluator | Value model, lowering, desugaring, evaluator, core + collections builtins used by the evaluator tests; `mash -c` and `mash script.mash`; builtin macro after ~10 builtins; port the golden evaluator tests |
| M3 | Pure stdlib + IDE logic | String/object/number methods, json, time, maths, hash, etc.; type inference; completion and assistance (tested without a terminal) |
| — | GC swap | Replace dumpster with the CPython-style collector before M4 (done) |
| M4 | Interactive shell | Line editor, screen, keys, history, completion UI, `!{…}`, `cd`, minimal os (ls, read, write, run) — daily-usable (done) |
| M5 | Remaining OS | Rest of os, users/groups, http, git, object browser (done) |
| M6 | Browser | `mash-web` with xterm.js (done) |

## Deliberate differences from Scala Mash (so far)

Small behaviour changes made during the port, each checked against the Scala implementation:

| Area | Scala Mash | Rust port |
|---|---|---|
| Selection expansion (M-Up) | Cursor just after a word selects the enclosing expression | Selects the word |
| `"~/$x"` (interpolated string starting with `~`) | Tilde silently dropped (`/a1`), apparently a bug: detected and stripped, but never expanded | Expanded to the home directory, as in `"~/foo"`; `` "`~/$x" `` for a literal `~` |
| Argument evaluation order | Hash-map order of parameters | Parameter order |
| `max`/`min` of all-null items | Crashes (`empty.max`) | The `--default` value |
| `naturalJoin` result order | Undefined (Scala `Set`) | Groups by first appearance in the target |
| `sort`/`sortBy --descending` with equal keys | Reverses an ascending sort, so equal elements come out in reverse order | Stable: equal elements keep their original order, as in Python (`reverse=True`), .NET `OrderByDescending`, Kotlin `sortedByDescending` and GNU `sort -s -r`; sorting twice then sorts by a second key |
| `countBy` | Not available | New: counts by key, as `groupBy` groups (`countBy identity xs` → `{ key: count }`; `--groups` for `{ key, count }` rows; `--all`, `--includeNull` as in `groupBy`); also a List and String method |
| `parseNumber 'abc'` | "Unexpected error … NumberFormatException" | "Could not parse 'abc' as a number" |
| `Character.isDigit` | Java `isDigit` | Unicode numeric (slightly broader) |
| `mash FILE`, `mash -e EXPR` | Not supported | Run a script / evaluate an expression (`mash -c` still runs via `sh`, as in Scala) |
| String indexes and lengths | UTF-16 units (`'😀'.length` is 2) | Characters (1), as agreed |
| `replace --regex` replacement | Java syntax (`$1x` is group 1 then `x`) | Rust syntax (`${1}x`; `$1x` is a group named `1x`), as agreed |
| `matches -i`, `grep -i` | ASCII-only case folding | Unicode case folding, as agreed |
| `'abc'.split ''` | `[a, b, c, ]` | `['', 'a', 'b', 'c', '']` (Rust semantics, as agreed) |
| `trim` | Java `trim` (chars ≤ U+0020) | Unicode whitespace |
| `now + 1.months` | Throws (unsupported unit) | Calendar arithmetic in the local zone, as agreed |
| `1.5.hours.fromNow` | Truncates to 1 hour | Exact; fractional days/weeks/months are errors |
| `now.format` | Locale long style (`9 October 2026 at 16:28:45 BST`) | Fixed English with zone abbreviation (`Fri 9 Oct 2026 12:00:00 UTC`), round-trips via `toDateTime` |
| `fuzzy` | Rounds (`47.hours` ago is "2 days ago") | Largest whole unit, truncated ("1 day ago"), as agreed |
| `toDateTime` | natty (garbage on partial matches) | Strict formats → normaliser → `parse_datetime`; never panics, null on failure |
| `json.fromString` | Lenient (Gson accepts `{a: 1}`) | Strict JSON; clean error messages |
| `json.prettyPrint` of NaN/Infinity | Writes `NaN` (invalid JSON) | Error, as agreed; cyclic values are errors too |
| `xml.fromString` | Coerces numbers/booleans, hash field order | Text kept as strings, document order, as agreed |
| `html.escape` | HTML 4 named entities for non-ASCII | Only `& < > "` |
| Errors from Java exceptions (`parseIso8601`, `base64.decode`, `1.to 5 --step=0`, bad regex, …) | "Unexpected error … SomeJavaException" | A clean Mash error message |
| `random.int` bounds | 32-bit | 64-bit (no 32-bit limit, as agreed) |
| `mash.eval` errors | Printed; returns unit | Propagate |
| Path completion under `/` | Common-prefix length off by one | Correct (affects only highlighting/common insertion) |
| Table and tree cell widths | UTF-16 units (wide characters misalign columns) | Display columns |
| Printing infinite numbers (`1/0`, alone or in tables) | `∞` (Java `DecimalFormat`) | `Infinity`, as `toString` |
| Viewing a list of units | Prints `[, ]` (its "print nothing" check never matches) | Prints nothing, as intended |
| `view.View.new` result | Viewing it fails ("No field 'print'") | A missing directive is false |
| `view.tree` of a cyclic value | Stack overflow | The repeated container is shown inline (`{…}`) |
| Cursor movement and deletion in the editor | By UTF-16 unit; wrapping and the cursor counted characters | By grapheme; rendering, wrapping and drawing count display columns (wide characters) |
| Ctrl-C at the prompt | SIGINT (the terminal kept `ISIG`) | Abandons the line, like Ctrl-G |
| Ctrl-Left / Ctrl-Right | Recognised but unbound | Move by word |
| Keys with no binding during history search | Became no-ops (and ended the search) | End the search and act as usual (e.g. Ctrl-A moves to the start) |
| Pasting | Typed key by key (newlines could run the line) | Bracketed paste inserts the text literally, newlines included |
| Status indicator for a `!` mish line | The whole line parsed as Mash (shown red) | The command after `!` parsed as mish |
| Startup tips | Include a disco-border tip | That tip omitted until disco borders exist; `config.view.discoBorders` ignored for now |
| REPL error printing | `ErrorPrinter` | Same format (kept over `MashError::report()`, which suits scripts: the REPL shows the highlighted source line with `^---` under it and no file name for the line just typed) |
| `"/".name` | NullPointerException | `null` |
| A failed `cd` | Still pushes the (unchanged) directory onto the history, so `back` stays put | History unchanged |
| `ls -r` order | File-system order | Sorted by name at each level (children follow their directory) |
| `glob` | Java `PathMatcher`, walked only one level unless the pattern has `**` (so `"a/*/b"` found nothing); `*` matches dotfiles | Per-segment matching at any depth; `*`, `**` skip dotfiles unless the segment starts with `.`; a final `**` matches everything below; symlink cycles are skipped |
| File errors (`cat "nope"`, `mkdir "x/y"`, …) | "Unexpected error … NoSuchFileException" | "Could not read 'nope': No such file or directory" |
| `"x".absolute` of a missing path | NoSuchFileException | "Could not resolve 'x': …" |
| `"x".withTilde` | String prefix (`/home/alicex` → `~x`) | Only at a path boundary |
| `"a".renameTo "b"` / `.copy` when the destination exists | Java exception | Clean error |
| `readLines`, `read`, `write` | Platform default charset | UTF-8 (invalid input decoded lossily) |
| Child processes | Same process group as Mash; stdin written before stdout read (deadlock risk) | Own process group with the terminal (`tcsetpgrp`), concurrent stdin/stdout, Ctrl-C in a child interrupts evaluation, Ctrl-Z resumes the child with a message (as planned) |
| `which` | `Files.isExecutable` (directories count) | Executable regular files only |
| HTTPS certificates (`http.*`, `json.get` etc.) | Every certificate and host name trusted, JVM-wide (`TrustAllTrustManager`) | Verified against the system's trust store; new `--insecure` flag on every request function to skip verification, as planned |
| Credentials on a redirect to another origin | `--basicAuth`, `Authorization` and `Cookie` headers sent on to any host a redirect names | Not sent once a redirect leaves the original scheme, host and port (as curl and Python's requests); other headers still are |
| Repeated request headers (`--headers=['A:1', 'A:2']`, `--headers={ A: [1, 2] }`) | Only the last is sent (`setHeader` replaces), defeating the support for several values | All are sent |
| Request errors | "Unexpected error … URISyntaxException" / "… ClientProtocolException" for bad or relative URLs | "Invalid URL: Illegal character …", "Invalid URL 'foo': must be an absolute http or https URL"; unknown host and connection errors keep Scala's wording ("Unknown host: …", "Couldn't connect to …") |
| `http.Response` field types | `headers` declared as `Object` and `headersObject`'s result as `[http.Header]` (swapped) | `headers: [http.Header]`, `headersObject: Object` (inference and completion only) |
| Form bodies (`--form`) | Encoded as ISO-8859-1 | UTF-8 |
| HTTP details | Apache client: `User-Agent: Apache-HttpClient/…`, `Accept-Encoding: gzip,deflate`, response header names as sent | ureq: its own `User-Agent`, `Accept: */*` if none given, gzip only; response header names capitalised from the `http` crate's lower case (`Content-Type`, but `Etag` for `ETag`) |
| `Response.json`, `json.get` etc. | Lenient Gson parsing (a body of `oops!` is the string `oops!`) | Strict JSON, as `json.fromString` |
| `dns.lookup`, `Host.ipAddresses` | IPv6 as `0:0:0:0:0:0:0:1` | `::1` (RFC 5952); IPv4 addresses still listed first, as Java does |
| `mash.source` errors | Printed; returns unit | Propagate (as `mash.eval`) |
| File errors in `json.read`, `xml.read`, `mash.source` | "Unexpected error … FileNotFoundException" | "Could not read 'x': No such file or directory" |
| Users and groups on macOS | Not available (`user`, `users`, `groups` missing; `Username` has no methods) | Available on Linux and macOS, from `getpwent`/`getgrent` (so on Linux, NSS users too, not just `/etc/passwd`), as planned |
| `Group.users`, `GroupInfo.users`, `UserSummary.groups` | A user can be listed twice (primary group that also lists them as a member) | Each listed once |
| `Gid.name` inferred type | `[core.Number tagged os.Username]` | `core.String tagged os.Group` (its value) |
| `UserSummary.uid` declared type | `core.String tagged os.Uid` (the value is a number) | `core.Number tagged os.Uid` |
| `Username`/`Uid`/`Pid` methods for an unknown user or process | `NoSuchElementException` (users) | "Could not find user 'x'", "Cannot find process with pid N" |
| `kill` given a list (`ps \| grep "java" \| kill`, its own documented example) | "Invalid process ID: [...]" | The list's processes are killed |
| `kill` errors | "Unable to kill process with pid N, error code -1" | "Unable to kill process with pid N: No such process" (the reason) |
| Signal names | jnr's per-OS list (e.g. `PWR`, `INFO`, `STKFLT` where defined) | The POSIX signals common to Linux and macOS (`os.Signal`, `kill --signal` completion); any number is accepted |
| `process.kill "…"` | No completion | Signal names complete, as for `kill` |
| `ps` on macOS | Only processes it can read (oshi) | All processes; the owner is null (and sizes 0) where the OS won't say |
| `terminal` | `NullPointerException` outside the REPL | "Not running in a terminal" when no standard stream is a terminal |
| `diskSpace` | Parses `df -k` with a regex (spaces in mount points and macOS's inode columns could confuse it) | Parses `df -kP` (POSIX format) by columns; same values |
| Clipboard on Linux | `xclip` only | `wl-copy`/`wl-paste` under Wayland, else `xclip`, else `xsel`; an error if none is installed |
| `clipboard`, `open` when the program fails | Ignored | An error |
| `history` without a REPL (`mash -e`, scripts) | `NullPointerException` | Empty |
| `jump` with equally used matches | Unspecified order | The last in path order |
| `os.DiskSpaceEntry` | Not a registered class | Registered, like the other os classes |
| git | JGit: Mash's own implementation of git (ignores hooks, much config; pushes into a non-bare repository's checked-out branch) | The `git` command: the user's git, config, hooks and credentials (as decided); git's own messages on stderr, then "git X failed (exit status N)" instead of JGit exception messages |
| Paths given to `git.add`/`stage`/`unstage`/`restore` | Relative to the top of the working tree (JGit) | Relative to the current directory, as git's; `--all` works from any subdirectory |
| `git.Status` path lists | Hash-set order | Sorted |
| `Tag.commit` of an annotated tag (`git.tag.create` makes one, as JGit's default) | The tag object's hash | The commit it points to |
| `Commit.diff` of a root commit | Crashes (`None.get`) | Every file added |
| `git.commit` message | Stored as given | git ends it with a newline (as messages of commits made with git already are) |
| `git.merge` with conflicts | Returns quietly | git reports the conflicts, then an error; `merge` and `pull` show git's output |
| `git.branch.current` with a detached HEAD | Crashes | "Not on a branch" |
| Printing a `git.Status` behind its upstream | "by 1 commits": pluralised by the ahead count | By the behind count |
| `RemoteBranchName.info` type | Inferred as `Branch` | `RemoteBranch` (what it returns) |
| Object browser: the expression for a field with a non-string key (`{ (1): … }`) | The key's expression with no brackets (`r01`) | `r0[1]` |
| Object browser: one marked field of an object, the cursor elsewhere | The marked field's expression with the current field's value (so focusing shows the wrong value) | The marked field's value |
| Object browser: hidden columns after the terminal is resized | Shown again (the table is rebuilt without them) | Stay hidden |
| Object browser: `t` on an empty list or object, `view.browse (view.tree x)` of an empty or non-container value | Crashes (a tree browser needs a first item) | Ignored; the usual view |
| Object browser search | Java regex, ASCII case folding | `fancy-regex`, Unicode case folding (as for `matches -i`) |
| Object browser: `o` (open) and `c` (copy) errors | Printed over the browser | Not printed |
| Object browser: Alt-E in the expression line | Evaluated with the real globals | Like other browser expressions, without changing the globals |

Printing results (`mash_repl::view`): output too large for the screen, and `view.browse`, open the object
browser.

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
| `spikes/vieworacle` | Not a spike: `mash-view 'expr'` prints what Scala Mash's `Viewer` shows for a value (`COLS=`, `FUZZY=0`, `ANSI=1`), for checking printer output. |
| `spikes/dates` | Baseline natty run: 45/54 correct but returns garbage on partial matches. Proposed pipeline 46/54 with 1 wrong; `interim` panics on our `format` output. |
