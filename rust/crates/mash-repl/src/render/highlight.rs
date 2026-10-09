//! Syntax highlighting of Mash code (Scala `MashRenderer`): token colours, plus unbound identifiers (bare
//! words), declared names, object keys, attributes, the bracket matching the one at the cursor, and the `!`
//! of a mish command.

use std::collections::HashSet;
use std::ops::Range;

use mash_core::ast::SourceUnit;
use mash_core::compile::Settings;
use mash_core::eval::Ctx;
use mash_syntax::ast::{AstNode, ClassDecl, FunctionDecl};
use mash_syntax::{Mode, SyntaxKind, TextRange};

use crate::commands::{mish_command, suffix_mish_command};
use crate::editor::matching_bracket;
use crate::screen::{Style, StyledChar, StyledString, default_colours as colours};

/// The colour of a kind of token (Scala `MashRenderer.getTokenStyle`)
pub fn token_style(kind: SyntaxKind) -> Style {
    use SyntaxKind::*;
    match kind {
        COMMENT => Style::fg(colours::COMMENT),
        NUMBER_LITERAL => Style::fg(colours::PURPLE),
        IDENTIFIER | MISH_WORD => Style::fg(colours::FOREGROUND),
        ERROR_TOKEN => Style::fg(colours::RED),
        LONG_FLAG | SHORT_FLAG => Style::fg(colours::CYAN),
        k if k.is_keyword() => Style::fg(colours::PINK),
        STRING_LITERAL | STRING_START | STRING_END | STRING_MIDDLE => Style::fg(colours::YELLOW),
        PIPE | PLUS | TIMES | SHORT_EQUALS | PLUS_EQUALS | MINUS_EQUALS | TIMES_EQUALS | DIVIDE_EQUALS
        | LONG_EQUALS | NOT_EQUALS | GREATER_THAN | GREATER_THAN_EQUALS | LESS_THAN_EQUALS | LESS_THAN | SEMI => {
            Style::fg(colours::ORANGE)
        }
        AT => Style::fg(colours::GREEN),
        _ => Style::DEFAULT,
    }
}

/// Options for highlighting.
#[derive(Clone, Copy, Debug, Default)]
pub struct Highlight {
    /// Unbound identifiers are strings (otherwise they're shown as errors)
    pub bare_words: bool,
    /// Mish by default
    pub mish: bool,
    /// Underline the bracket matching the one at this offset
    pub cursor: Option<usize>,
}

/// `text` highlighted: one styled character per character of `text`. With `ctx`, identifiers not bound in
/// its globals are shown as bare words (strings) or errors.
pub fn highlight(ctx: Option<&mut Ctx>, text: &str, options: Highlight) -> StyledString {
    // The part of the text that's code, whether it's mish, and the styled prefix and suffix around it
    let (code_range, mish) = if let Some(code) = suffix_mish_command(text) {
        (0..code.len(), true)
    } else if let Some((prefix_len, _)) = mish_command(text) {
        (prefix_len..text.len(), true)
    } else {
        (0..text.len(), options.mish)
    };
    let code = &text[code_range.clone()];
    let mut out = StyledString::new();
    out.push_str(&text[..code_range.start], Style::DEFAULT.with_bold());

    let tokens = mash_syntax::lexer::tokenise(code, mish).tokens;
    let parse = mash_syntax::parse(code, if mish { Mode::Mish } else { Mode::Mash });
    let special = special_tokens(&parse.syntax());
    let bare: HashSet<TextRange> = match ctx {
        Some(ctx) => {
            let settings = Settings { bare_words: true, mish };
            let compiled = ctx.compile_forgiving_with(SourceUnit::new("<highlight>", code), settings);
            compiled.bare_words.into_iter().collect()
        }
        None => HashSet::new(),
    };
    let bracket = options
        .cursor
        .and_then(|c| c.checked_sub(code_range.start))
        .filter(|&c| c <= code.len())
        .and_then(|c| matching_bracket(code, c, mish));

    for token in tokens {
        let style = if bare.contains(&token.range) {
            if options.bare_words {
                token_style(SyntaxKind::STRING_LITERAL)
            } else {
                Style::fg(colours::RED)
            }
        } else {
            match special.iter().find(|(range, _)| *range == token.range) {
                Some((_, Special::DeclaredName)) => token_style(token.kind).with_bold(),
                Some((_, Special::ObjectKey)) => token_style(SyntaxKind::STRING_LITERAL),
                Some((_, Special::Attribute)) => token_style(SyntaxKind::AT),
                None => token_style(token.kind),
            }
        };
        let start = usize::from(token.range.start());
        for (i, c) in code[byte_range(token.range)].char_indices() {
            let style = if Some(start + i) == bracket {
                style.with_underline()
            } else {
                style
            };
            out.push(StyledChar::new(c, style));
        }
    }
    out.push_str(&text[code_range.end..], Style::DEFAULT.with_bold());
    out
}

/// Restyle the characters of `styled` (one per character of `text`) in the byte range `range`
pub(crate) fn restyle(styled: &mut StyledString, text: &str, range: Range<usize>, f: impl Fn(Style) -> Style) {
    for (index, (offset, _)) in text.char_indices().enumerate() {
        if range.contains(&offset)
            && let Some(c) = styled.chars.get_mut(index)
        {
            c.style = f(c.style);
        }
    }
}

fn byte_range(range: TextRange) -> Range<usize> {
    usize::from(range.start())..usize::from(range.end())
}

enum Special {
    DeclaredName,
    ObjectKey,
    Attribute,
}

/// Tokens highlighted for their role: names of functions and classes being declared, bare object keys and
/// attribute names
fn special_tokens(root: &mash_syntax::SyntaxNode) -> Vec<(TextRange, Special)> {
    use SyntaxKind::*;
    let first_identifier = |node: &mash_syntax::SyntaxNode| {
        node.children_with_tokens()
            .filter_map(|e| e.into_token())
            .find(|t| t.kind() == IDENTIFIER)
            .map(|t| t.text_range())
    };
    let mut special = vec![];
    for node in root.descendants() {
        let found = match node.kind() {
            FUNCTION_DECL => FunctionDecl::cast(node.clone())
                .and_then(|d| d.name_token())
                .map(|t| (t.text_range(), Special::DeclaredName)),
            CLASS_DECL => ClassDecl::cast(node.clone())
                .and_then(|d| d.name_token())
                .map(|t| (t.text_range(), Special::DeclaredName)),
            SHORTHAND_OBJECT_ENTRY => first_identifier(&node).map(|r| (r, Special::ObjectKey)),
            FULL_OBJECT_ENTRY => node
                .children()
                .next()
                .filter(|field| field.kind() == IDENTIFIER_EXPR)
                .and_then(|field| first_identifier(&field))
                .map(|r| (r, Special::ObjectKey)),
            ATTRIBUTE => first_identifier(&node).map(|r| (r, Special::Attribute)),
            _ => None,
        };
        special.extend(found);
    }
    special
}

/// Render text with `<mash>…</mash>` regions highlighted as Mash code (Scala `MashMarkupRenderer`)
pub fn render_markup(s: &str) -> StyledString {
    let mut out = StyledString::new();
    let mut rest = s;
    while let Some(start) = rest.find("<mash>")
        && let Some(length) = rest[start + 6..].find("</mash>")
    {
        out.push_str(&rest[..start], Style::DEFAULT);
        let code = &rest[start + 6..start + 6 + length];
        let code = code.strip_prefix('\r').unwrap_or(code);
        let code = code.strip_prefix('\n').unwrap_or(code);
        let code = code.strip_suffix('\n').unwrap_or(code);
        let code = code.strip_suffix('\r').unwrap_or(code);
        out.append(&highlight(None, code, Highlight::default()));
        rest = &rest[start + 6 + length + 7..];
    }
    out.push_str(rest, Style::DEFAULT);
    out
}
