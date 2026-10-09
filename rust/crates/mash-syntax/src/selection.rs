//! Syntax-aware selection expansion (M-Up in the REPL). Port of `editor/SyntaxSelection.scala`.
//!
//! Candidates are the ranges of all syntax nodes (declarations extended to include their doc comment) and
//! of "selectable" tokens (keywords, comments, identifiers, flags). The expansion is the smallest candidate
//! that contains the current selection and is strictly longer, falling back to the whole text.

use crate::{SyntaxKind, SyntaxNode, TextRange, TextSize, ast};
use rowan::NodeOrToken;

fn is_selectable_token(kind: SyntaxKind) -> bool {
    kind.is_keyword()
        || matches!(
            kind,
            SyntaxKind::COMMENT | SyntaxKind::IDENTIFIER | SyntaxKind::LONG_FLAG | SyntaxKind::SHORT_FLAG
        )
}

/// Region containment as the editor sees it: ranges are half-open, so a cursor (empty range) at the very
/// end of a token is *not* inside it.
fn contains(outer: TextRange, inner: TextRange) -> bool {
    let contains_pos = |pos: TextSize| outer.start() <= pos && pos < outer.end();
    if inner.is_empty() {
        contains_pos(inner.start())
    } else {
        contains_pos(inner.start()) && contains_pos(inner.end() - TextSize::from(1))
    }
}

/// The smallest strictly-longer range containing `selection` (an empty range is a cursor), or None if the
/// selection can't grow.
pub fn expand_selection(root: &SyntaxNode, selection: TextRange) -> Option<TextRange> {
    let whole = root.text_range();
    candidates(root)
        .filter(|candidate| contains(*candidate, selection) && candidate.len() > selection.len())
        .min_by_key(|candidate| candidate.len())
        .or(Some(whole))
        .filter(|range| *range != selection)
}

fn candidates(root: &SyntaxNode) -> impl Iterator<Item = TextRange> {
    root.descendants_with_tokens().filter_map(|element| match element {
        NodeOrToken::Token(t) if is_selectable_token(t.kind()) => {
            // Comment tokens include their line ending; don't select that
            let text = t.text();
            let trimmed = text.trim_end_matches(['\r', '\n']);
            Some(TextRange::at(t.text_range().start(), TextSize::of(trimmed)))
        }
        NodeOrToken::Token(_) => None,
        NodeOrToken::Node(n) => {
            let range = n.text_range();
            if matches!(n.kind(), SyntaxKind::FUNCTION_DECL | SyntaxKind::CLASS_DECL)
                && let Some(first_comment) = ast::doc_comment_tokens(&n).first()
            {
                Some(TextRange::new(first_comment.text_range().start(), range.end()))
            } else {
                Some(range)
            }
        }
    })
}
