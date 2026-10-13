# Test porting tracker

Maps each Scala test file (`src/test/scala/com/github/mdr/mash/…`) to its Rust counterpart. Status:
**ported** (all cases), **partial** (see notes), **pending** (milestone where it's due), or **n/a**.

## M1 — syntax (`mash-syntax`)

| Scala test | Rust | Status | Notes |
|---|---|---|---|
| `lexer/MashLexerTest` | `mash-syntax/tests/lexer.rs` | ported | Strict-mode cases check that an error is reported (the Rust lexer never throws); error positions aren't compared |
| `lexer/IsLegalIdentifierTest` | `mash-syntax/tests/lexer.rs` | ported | |
| `parser/MashParserTest` | `mash-syntax/tests/parser.rs`, `tests/common/mod.rs` | ported | "Should not crash" cases are in the robustness corpus (every prefix, random deletions) |
| `parser/StringEscapesTest` | `mash-syntax/src/escapes.rs` | ported | |
| `parser/ExpressionCombinerTest` | `mash-syntax/src/expression_combiner.rs` | ported | |
| `parser/LookupDecomposerTest` | `mash-syntax/src/lookup_decomposer.rs` | ported | |
| `editor/SyntaxSelectionTest` | `mash-syntax/tests/selection.rs` | ported | |
| `evaluator/DocCommentsTest` | `mash-syntax/tests/doc_comments.rs` | partial | Syntax side (which comment attaches to which declaration) ported; evaluation (`.summary`, `.description`) due in M2 |
| — | `mash-syntax/tests/limits.rs` | new | Nesting limit, deep and long-chain inputs |
| — | `mash-syntax/tests/completion.rs` | new | Finding the completion target in forgiving parses |

## M2/M3 — evaluator, standard library and IDE logic (`mash-core`)

| Scala test | Rust | Status | Notes |
|---|---|---|---|
| `parser/AbstractifierTest` | `mash-core/tests/compile.rs` | ported | Plus lowering of each kind of syntax |
| `compiler/BareStringifyTest`, `DesugarHolesTest`, `DesugarPipesTest`, `ParenRemoverTest` | `mash-core/tests/compile.rs` | ported | |
| — | `mash-core/tests/value_{leaks,keys,reentrancy}.rs` | new | From the values spike: cycles are collected, key semantics, no `RefCell` re-entrancy panics |
| `evaluator/*Test` (44 files) | `mash-core/tests/evaluator/<name>.rs` | partial | 921 cases converted mechanically (counts checked per file); all pass. Cases needing later milestones go in their module's `DEFERRED` with the error they hit; a deferred case that starts passing fails the test, so the lists stay honest. `SemicolonInferenceTest` is a Scala `object` that ScalaTest never ran |
| — | `mash-core/tests/evaluator/{strings_m3,time,namespaces,strings}.rs` | new | Regex and string semantics, time arithmetic/parsing/formatting, json/xml/maths/net, tilde expansion; checked against Scala Mash except where a comment marks a deliberate difference |
| — | `mash-core/tests/evaluator/core_builtins.rs` | new | 32 cases for core builtins the Scala tests don't cover (expected values checked against Scala Mash) |
| — | `mash-core/tests/eval.rs` | new | Error locations and stack traces, depth limit, interrupts, GC under evaluation |
| — | `mash-cli/tests/cli.rs` | new | `mash -e`, scripts, `mash -c` via sh |

| Scala test | Rust | Status | Notes |
|---|---|---|---|
| `evaluator/SimpleEvaluatorTest` | `mash-core/tests/infer_misc.rs` | ported | Constant folding agrees with evaluation |
| `functions/BindTypesTest` | `mash-core/tests/infer_misc.rs` | ported | |
| `language/ValueToExpressionTest` | `mash-core/tests/value_to_expression.rs` | ported | |
| `inference/TypeInferencerTest` | `mash-core/tests/infer.rs` | ported | All 257 pass |
| `inference/ValueTypeDetectorTest` | `mash-core/tests/infer_misc.rs` | ported | The class-instance case uses `maths.Stats` instead of the os class `PermissionsSection` |
| — | `mash-core/tests/infer_{collections,core,namespaces}.rs` | new | Inference rules and completion specs per builtin area (`infer_namespaces.rs` includes http, dns, csv and the json/xml file and HTTP functions) |
| `completions/CompletionsTest` | `mash-core/tests/complete.rs` | ported | 149 cases. Plus 10 new cases: file completion for `json.read`/`write`, `xml.read`, `csv.write`, `mash.source`; members of `http.Response`, `net.Host`; `--insecure` |
| `completions/ContiguousRegionFinderTest`, `InvocationFinderTest`, `PathCompleterTest` | `mash-core/tests/complete.rs` | ported | |
| `assist/InvocationAssistanceTest` | `mash-core/tests/complete.rs` | ported (adapted) | Same cursor positions, with core functions in place of `ls`/`home` (M5) |
| `evaluator/TildeExpanderTest` | | pending (M4) | Expansion is ported with path completion; retilde-ing for display comes with the REPL |
| `WorkingDirectoryStackTest` | `mash-core/src/builtins/os/dirs.rs` | ported | |
| `os/GlobHelperTest` | `mash-core/src/builtins/os/glob.rs` | ported | Plus segment-matching tests; glob semantics changed (see PORT_PLAN.md) |
| — | `mash-core/tests/os.rs` | new | os builtins, path methods, `!{…}`/`!!{…}`/`!cmd`/mish lines, `run`, `env` as the child environment, Ctrl-C in a child — against `MemoryFiles` and fake commands |
| — | `mash-core/tests/git.rs` | new | The `git` namespace against a fake `git` (output captured from git for a scratch repository): objects parsed from status/log/for-each-ref/…, the commands run for each change, errors, completion (branches, tags, remotes, status paths) and inference; checked against Scala Mash for the same repository |
| — | `mash-os/tests/git.rs` | new | The real `git` in temporary repositories (isolated config, fixed identity and dates): init, staging, committing, amending, log/diff, branches, merging, stashing, tags, cloning, pushing, fetching and pulling |
| — | `mash-os/tests/native.rs`, `native_cd.rs` | new | Native processes (capture, exit status, SIGINT, stdin, 4 MB through `cat` without deadlock, redirects, environment) and file operations in temp directories |
| — | `mash-core/tests/net.rs` | new | http (requests sent: headers, cookies, basic auth, bodies, forms, files, query parameters; responses and their methods; argument errors), `json.get` etc., dns, `Url.open`, and the data file functions (`json.read`/`write`, `xml.read`, `csv.write`, `hash.sha256` of a file, `mash.source`, `mash.reload`) — against `TestPlatform`'s canned responses and in-memory files; checked against Scala Mash with a local HTTP server |
| — | `mash-os/tests/native_net.rs` | new | The native HTTP client against local servers: headers, cookies, basic auth, bodies, status codes, redirects (and POST not redirected by 302), connection refused, an HTTPS server with a self-signed certificate (refused unless `--insecure`), `localhost` DNS. `mash-os/src/net.rs` unit tests: TLS configuration, redirect locations, `Set-Cookie` parsing |
| — | `mash-cli/src/startup.rs` (unit tests) | new | `mash.reload` with the shell's hook: builtins reset, libraries and `init.mash` run again |
| — | `mash-core/tests/os_system.rs` | new | Users and groups, processes and `kill`, clipboard, `open`, disk space, terminal, `history`, `jump`, their completion and inference, and failing cleanly without platform support — against `TestPlatform`'s `FakeSystem` and a recorded history |
| — | `mash-os/tests/native_system.rs` | new | The current user, groups, the test's own process in `ps`, disk space for `/`, and `kill` of a child the test starts (read-only otherwise: no clipboard, `open` or terminal) |
| — | `mash-repl/tests/history_access.rs` | new | `history` and `jump` see the REPL's history, with this session's results |
| `utils/*` (`LineInfoTest`, `PrefixTreeTest`, `RaggedGridNavigatorTest`, `RegionTest`, `StringUtilsTest`, `UtilsTest`) | | pending | Ported alongside the code that uses them |

## M4/M5 — REPL and OS

| Scala test | Rust | Status | Notes |
|---|---|---|---|
| `editor/BracketMatcherTest`, `QuoteTogglerTest` | `mash-repl/tests/editor.rs` | ported | |
| `repl/LineBufferTest` | `mash-repl/tests/line_buffer.rs` | ported | Plus graphemes and wide characters |
| `repl/handler/InsertLastArgHandlerTest`, `IncrementalHistorySearchActionHandlerTest` | `mash-repl/tests/handlers.rs` | ported | Timestamps come from a ticking test clock instead of being patched afterwards |
| `repl/history/HistoryTest` (+ `InMemoryHistoryStorage`) | `mash-repl/tests/handlers.rs` | ported | `MemoryHistoryStorage` |
| `screen/ScreenDrawerTest` (+ `terminal/InMemoryTerminal`) | `mash-repl/tests/screen_drawer.rs`, `tests/common/mod.rs` | ported | Plus erasing, wide characters, accepting; the in-memory terminal understands all SGR codes and wide characters |
| `view/render/AssistanceRendererTest`, `IncrementalHistorySearchRendererTest`, `CallingSyntaxRendererTest` | `mash-repl/tests/render.rs` | ported | |
| `integration/MiscIntegrationTest`, `IncrementalCompletionIntegrationTest`, `ReplRendererIntegrationTest` (+ `AbstractIntegrationTest`, `DummyTerminal`) | `mash-repl/tests/integration.rs`, `tests/browser.rs`, `tests/common/mod.rs` | ported | The object-browser cases ("Invocation assistance should work in expression browser", "Empty lines should be rendered in browser expression editor") are in `browser.rs`. Plus new tests: printing results and errors, interrupts, mish toggling, key events, paste, Ctrl-D, startup tips, and drawing every step into the in-memory terminal |
| `view/printer/ColumnAllocatorTest` | `mash-repl/tests/columns.rs` | ported | Plus cases for the other branches |
| `view/render/help/*Test` (class, field, function, method) | `mash-repl/tests/help.rs` | ported | The `TestPointClass` fixture becomes the equivalent `ClassDoc`/`FieldDoc`/… descriptions |
| `view/render/CallingSyntaxRendererTest` (help) | `mash-repl/tests/help.rs` | ported | |
| — | `mash-repl/tests/view.rs` | new | 76 golden cases for `view_value` (tables, objects, trees, help (including `http.Response` and `http.post?`), `view.*`); expected output from Scala Mash's `Viewer` except where a comment marks a deliberate difference; styles checked too |
| — | `mash-repl/tests/git_view.rs` | new | `git.Status` printed like `git status` (Scala `GitStatusPrinter`, with colours) and commit tables; expected output from Scala Mash's `Viewer` |
| `repl/browser/TwoDTableBrowserStateTest` | `mash-repl/tests/browser.rs` | ported | |
| `integration/BrowserIntegrationTest` | `mash-repl/tests/browser.rs` | ported | |
| — | `mash-repl/tests/browser.rs` | new | 36 tests: opening the browser (large output, `browseLargeOutput`, `view.browse`/`print`/`tree`, Ctrl-V), keys and key maps, rows, pages, columns, focus and back, marking, hiding columns, the tree, 1D/2D views, help links, search, expressions (follow-on, replacement, globals untouched, completion), inserting into the command line, files and directories, rendering each view (screens from Scala Mash's browser renderers, styles), and drawing on the alternate screen |
| — | `mash-repl/src/browser/search.rs` | new | Search matching (character ranges, invalid regexes as literal text) |

## M6 — the browser

No Scala tests (Scala Mash had no browser build).

| Scala test | Rust | Status | Notes |
|---|---|---|---|
| — | `mash-web/tests/shell.rs` | new | The browser's platform and shell natively, with a fake bridge: sample files, the one user, what the browser can't do failing cleanly, output, HTTP, clipboard and URLs through the bridge, the time zone, interrupts, a session (commands, history saved and recalled, Ctrl-D) |
| — | `mash-web/tests/shell.rs` (`share_…`, `run_command_…`) | new | `share` links this session's commands (not earlier sessions'), percent-encoded, last n, copied to the clipboard; running a command as if typed, and waiting while the object browser is open |
| — | `mash-web/src/keys.rs` (unit tests) | new | Browser keyboard events as key presses: named keys, Shift, Ctrl, Option on macOS, AltGr, keys left to the browser |
| — | `mash-web/www/test/wasm.mjs` | new | The wasm build in Node: evaluation, printing, the bundled time zone database, interrupts, HTTP errors, deep recursion stopping cleanly (and Mash usable afterwards) |
| — | `mash-web/www/test/browser.mjs` | new | Deep recursion (`recursion.mjs`: plain, `map`, methods, `sortBy`, blocks) stops cleanly in headless Chromium, Firefox and WebKit, and how deep each allows |
| — | `mash-web/www/test/page.mjs` | new | The page in headless Chromium, Firefox and WebKit: commands, Ctrl-C (variables kept), Ctrl-C without cross-origin isolation (a new session), isolation by the service worker where there are no headers (Ctrl-C keeps variables), history across reloads, the object browser, Ctrl-D and starting again; links: their commands run only if asked, wait for the object browser, stop on Ctrl-C, and `share` makes a link that reruns a session |
| — | `mash-core/tests/eval.rs` | new | `the_platform_can_interrupt_evaluation`: interrupts asked of the platform, while evaluating and sleeping |
