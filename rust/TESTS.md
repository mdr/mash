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
| `evaluator/*Test` (44 files) | `mash-core/tests/evaluator/<name>.rs` | partial | 921 cases converted mechanically (counts checked per file). 920 pass; the other 1 needs git (M5) and are listed in their module's `DEFERRED` with the error they hit. A deferred case that starts passing fails the test, so the lists stay honest. `SemicolonInferenceTest` is a Scala `object` that ScalaTest never ran |
| — | `mash-core/tests/evaluator/{strings_m3,time,namespaces,strings}.rs` | new | Regex and string semantics, time arithmetic/parsing/formatting, json/xml/maths/net, tilde expansion; checked against Scala Mash except where a comment marks a deliberate difference |
| — | `mash-core/tests/evaluator/core_builtins.rs` | new | 32 cases for core builtins the Scala tests don't cover (expected values checked against Scala Mash) |
| — | `mash-core/tests/eval.rs` | new | Error locations and stack traces, depth limit, interrupts, GC under evaluation |
| — | `mash-cli/tests/cli.rs` | new | `mash -e`, scripts, `mash -c` via sh |

| Scala test | Rust | Status | Notes |
|---|---|---|---|
| `evaluator/SimpleEvaluatorTest` | `mash-core/tests/infer_misc.rs` | ported | Constant folding agrees with evaluation |
| `functions/BindTypesTest` | `mash-core/tests/infer_misc.rs` | ported | |
| `language/ValueToExpressionTest` | `mash-core/tests/value_to_expression.rs` | ported | |
| `inference/TypeInferencerTest` | `mash-core/tests/infer.rs` | partial | 251 of 257 pass; 6 need users/git/http (M5), listed in `DEFERRED` |
| `inference/ValueTypeDetectorTest` | `mash-core/tests/infer_misc.rs` | ported | The class-instance case uses `maths.Stats` instead of the os class `PermissionsSection` |
| — | `mash-core/tests/infer_{collections,core,namespaces}.rs` | new | Inference rules and completion specs per builtin area |
| `completions/CompletionsTest` | `mash-core/tests/complete.rs` | partial | 149 cases; 146 pass, 3 need `kill`/`user` (M5) and are listed in `DEFERRED` |
| `completions/ContiguousRegionFinderTest`, `InvocationFinderTest`, `PathCompleterTest` | `mash-core/tests/complete.rs` | ported | |
| `assist/InvocationAssistanceTest` | `mash-core/tests/complete.rs` | ported (adapted) | Same cursor positions, with core functions in place of `ls`/`home` (M5) |
| `evaluator/TildeExpanderTest` | | pending (M4) | Expansion is ported with path completion; retilde-ing for display comes with the REPL |
| `WorkingDirectoryStackTest` | `mash-core/src/builtins/os/dirs.rs` | ported | |
| `os/GlobHelperTest` | `mash-core/src/builtins/os/glob.rs` | ported | Plus segment-matching tests; glob semantics changed (see PORT_PLAN.md) |
| — | `mash-core/tests/os.rs` | new | os builtins, path methods, `!{…}`/`!!{…}`/`!cmd`/mish lines, `run`, `env` as the child environment, Ctrl-C in a child — against `MemoryFiles` and fake commands |
| — | `mash-os/tests/native.rs`, `native_cd.rs` | new | Native processes (capture, exit status, SIGINT, stdin, 4 MB through `cat` without deadlock, redirects, environment) and file operations in temp directories |
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
| `integration/MiscIntegrationTest`, `IncrementalCompletionIntegrationTest`, `ReplRendererIntegrationTest` (+ `AbstractIntegrationTest`, `DummyTerminal`) | `mash-repl/tests/integration.rs`, `tests/common/mod.rs` | partial | All but the object-browser cases ("Invocation assistance should work in expression browser", "Empty lines should be rendered in browser expression editor"), due in M5. Plus new tests: printing results and errors, interrupts, mish toggling, key events, paste, Ctrl-D, startup tips, and drawing every step into the in-memory terminal |
| `view/printer/ColumnAllocatorTest` | `mash-repl/tests/columns.rs` | ported | Plus cases for the other branches |
| `view/render/help/*Test` (class, field, function, method) | `mash-repl/tests/help.rs` | ported | The `TestPointClass` fixture becomes the equivalent `ClassDoc`/`FieldDoc`/… descriptions |
| `view/render/CallingSyntaxRendererTest` (help) | `mash-repl/tests/help.rs` | ported | |
| — | `mash-repl/tests/view.rs` | new | 74 golden cases for `view_value` (tables, objects, trees, help, `view.*`); expected output from Scala Mash's `Viewer` except where a comment marks a deliberate difference; styles checked too |
| `repl/browser/TwoDTableBrowserStateTest` | | pending (M5) | |
| `integration/*` (object browser) | | pending (M5) | |
