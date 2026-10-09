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

## M2 — evaluator (`mash-core`)

| Scala test | Rust | Status | Notes |
|---|---|---|---|
| `parser/AbstractifierTest` | `mash-core/tests/compile.rs` | ported | Plus lowering of each kind of syntax |
| `compiler/BareStringifyTest`, `DesugarHolesTest`, `DesugarPipesTest`, `ParenRemoverTest` | `mash-core/tests/compile.rs` | ported | |
| — | `mash-core/tests/value_{leaks,keys,reentrancy}.rs` | new | From the values spike: cycles are collected, key semantics, no `RefCell` re-entrancy panics |
| `evaluator/*Test` (44 files) | `mash-core/tests/evaluator/<name>.rs` | partial | 921 cases converted mechanically (counts checked per file). 676 pass; 245 are listed in each module's `DEFERRED` with the error they hit, all missing M3 builtins (String/Number/Object methods, time, json, regex, maths, net) or M5 (`pwd`, `git`). A deferred case that starts passing fails the test, so the lists stay honest. `SemicolonInferenceTest` is a Scala `object` that ScalaTest never ran |
| — | `mash-core/tests/evaluator/core_builtins.rs` | new | 32 cases for core builtins the Scala tests don't cover (expected values checked against Scala Mash) |
| — | `mash-core/tests/eval.rs` | new | Error locations and stack traces, depth limit, interrupts, GC under evaluation |
| — | `mash-cli/tests/cli.rs` | new | `mash -e`, scripts, `mash -c` via sh |

| Scala test | Status |
|---|---|
| `evaluator/SimpleEvaluatorTest` | pending (M3): it tests the inferencer's constant folding |
| `evaluator/TildeExpanderTest` | pending (M4): retilde-ing is only used by the REPL |
| `functions/BindTypesTest` | pending (M3): type binding for inference |
| `language/ValueToExpressionTest` | pending (M3): `mash.expressionFor` |
| `WorkingDirectoryStackTest` | pending (M4) |
| `utils/*` (`LineInfoTest`, `PrefixTreeTest`, `RaggedGridNavigatorTest`, `RegionTest`, `StringUtilsTest`, `UtilsTest`) | pending; ported alongside the code that uses them |

## M3 — IDE logic

| Scala test | Status |
|---|---|
| `inference/TypeInferencerTest`, `ValueTypeDetectorTest` | pending (M3) |
| `completions/*` (`CompletionsTest`, `ContiguousRegionFinderTest`, `InvocationFinderTest`, `PathCompleterTest`) | pending (M3) |
| `assist/InvocationAssistanceTest` | pending (M3) |

## M4/M5 — REPL and OS

| Scala test | Status |
|---|---|
| `editor/BracketMatcherTest`, `QuoteTogglerTest` | pending (M4) |
| `repl/LineBufferTest`, `repl/handler/*`, `repl/history/HistoryTest` | pending (M4) |
| `screen/ScreenDrawerTest` | pending (M4) |
| `view/**` (renderers, help renderers, `ColumnAllocatorTest`) | pending (M4/M5) |
| `repl/browser/TwoDTableBrowserStateTest` | pending (M5) |
| `integration/*` | pending (M4/M5) |
| `os/GlobHelperTest` | pending (M5); glob semantics change (see PORT_PLAN.md) |
