//! Lexer and lossless, error-tolerant parser for the Mash language.
//!
//! [`parse`] turns text into a `rowan` syntax tree that reproduces the input exactly (including whitespace
//! and comments). Parsing never fails: problems are reported as [`SyntaxError`]s, and the tree contains
//! `ERROR` nodes or simply lacks missing children. [`ast`] provides typed accessors over the tree.
//!
//! Lowering to the evaluator's AST lives in `mash-core`.

pub mod ast;
pub mod doc_comment;
pub mod escapes;
pub mod expression_combiner;
pub mod lexer;
pub mod lookup_decomposer;
mod parser;
pub mod selection;
mod syntax_kind;

use ast::AstNode as _;
pub use rowan::{TextRange, TextSize};
pub use syntax_kind::{MashLanguage, SyntaxElement, SyntaxKind, SyntaxNode, SyntaxNodePtr, SyntaxToken};

/// Maximum nesting depth of the syntax tree. Each level of bracket nesting (parentheses, lists, blocks,
/// interpolations) counts as one, as does each link in a chain that nests the tree (`a + b + …`, `a.b.…`,
/// `a | b | …`). Deeper input is reported as an error rather than overflowing the native stack, here or in
/// later passes over the tree.
pub const MAX_NESTING_DEPTH: u32 = 1_000;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxError {
    pub message: String,
    pub range: TextRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Mode {
    /// Normal Mash code
    #[default]
    Mash,
    /// A whole line of mish (shell) syntax, as in the REPL's mish mode
    Mish,
}

/// The result of parsing: a lossless syntax tree plus any errors.
#[derive(Debug, Clone)]
pub struct Parse {
    green: rowan::GreenNode,
    errors: Vec<SyntaxError>,
}

impl Parse {
    pub fn green(&self) -> &rowan::GreenNode {
        &self.green
    }

    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green.clone())
    }

    pub fn source_file(&self) -> ast::SourceFile {
        ast::SourceFile::cast(self.syntax()).expect("root is always a SOURCE_FILE")
    }

    /// Lexer and parser errors, in source order. Empty means the input is valid Mash.
    pub fn errors(&self) -> &[SyntaxError] {
        &self.errors
    }

    pub fn ok(&self) -> bool {
        self.errors.is_empty()
    }

    /// Indented tree dump, for tests and debugging
    pub fn debug_tree(&self) -> String {
        debug_tree(&self.syntax())
    }
}

/// Parse `text` as Mash code, or as a line of mish (shell) syntax.
pub fn parse(text: &str, mode: Mode) -> Parse {
    let lexed = lexer::tokenise(text, mode == Mode::Mish);
    let mut parser = parser::Parser::new(text, &lexed.tokens);
    match mode {
        Mode::Mash => parser.source_file(),
        Mode::Mish => parser.mish_file(),
    }
    let (green, mut errors) = parser.finish();
    let mut all = lexed.errors;
    all.append(&mut errors);
    all.sort_by_key(|e| e.range.start());
    Parse { green, errors: all }
}

/// Indented dump of a syntax tree (non-trivia tokens only), for tests and debugging.
pub fn debug_tree(node: &SyntaxNode) -> String {
    use std::fmt::Write;
    let mut out = String::new();
    let mut indent = 0;
    for event in node.preorder_with_tokens() {
        match event {
            rowan::WalkEvent::Enter(element) => {
                match &element {
                    rowan::NodeOrToken::Node(n) => writeln!(
                        out,
                        "{:indent$}{:?}@{:?}",
                        "",
                        n.kind(),
                        n.text_range(),
                        indent = indent
                    )
                    .unwrap(),
                    rowan::NodeOrToken::Token(t) => {
                        if !t.kind().is_trivia() {
                            writeln!(out, "{:indent$}{:?} {:?}", "", t.kind(), t.text(), indent = indent).unwrap()
                        }
                    }
                }
                indent += 2;
            }
            rowan::WalkEvent::Leave(_) => indent -= 2,
        }
    }
    out
}
