//! Lexer and lossless, error-tolerant parser for the Mash language.
//!
//! Text is turned into a `rowan` syntax tree that reproduces the input exactly (including whitespace
//! and comments), with typed accessors on top. Lowering to the evaluator's AST lives in `mash-core`.
