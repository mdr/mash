//! Syntax highlighting of Mash code, for help text (Scala `MashRenderer`, without the editor-only parts:
//! bare words, bracket matching and mish commands).

use mash_syntax::{Mode, SyntaxKind, SyntaxKind::*, SyntaxToken};

use crate::screen::{Style, StyledString, default_colours as colours};

/// The style of a kind of token (Scala `MashRenderer.getTokenStyle`)
pub fn token_style(kind: SyntaxKind) -> Style {
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

/// `source` (Mash code, which needn't parse) with each token in its style. Declared function and class names
/// are bold; bare object keys look like strings; attribute names look like `@`.
pub fn highlight(source: &str) -> StyledString {
    let parse = mash_syntax::parse(source, Mode::Mash);
    let mut result = StyledString::new();
    for token in parse.syntax().descendants_with_tokens().filter_map(|e| e.into_token()) {
        result.push_str(token.text(), style_in_context(&token));
    }
    result
}

fn style_in_context(token: &SyntaxToken) -> Style {
    let style = token_style(token.kind());
    if token.kind() != IDENTIFIER {
        return style;
    }
    let Some(parent) = token.parent() else {
        return style;
    };
    match parent.kind() {
        FUNCTION_DECL | CLASS_DECL => style.with_bold(),
        SHORTHAND_OBJECT_ENTRY => token_style(STRING_LITERAL),
        ATTRIBUTE => token_style(AT),
        // `{ key: value }`: the key is the entry's first child
        IDENTIFIER_EXPR
            if parent.parent().is_some_and(|entry| {
                entry.kind() == FULL_OBJECT_ENTRY && entry.first_child().as_ref() == Some(&parent)
            }) =>
        {
            token_style(STRING_LITERAL)
        }
        _ => style,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn style_at(s: &StyledString, i: usize) -> Style {
        s.chars[i].style
    }

    #[test]
    fn highlights_tokens() {
        let s = highlight("def square n = n * 2 # double");
        assert_eq!(s.forget_styling(), "def square n = n * 2 # double");
        assert_eq!(style_at(&s, 0), Style::fg(colours::PINK));
        assert_eq!(style_at(&s, 4), Style::fg(colours::FOREGROUND).with_bold());
        assert_eq!(style_at(&s, 11), Style::fg(colours::FOREGROUND));
        assert_eq!(style_at(&s, 13), Style::fg(colours::ORANGE));
        assert_eq!(style_at(&s, 19), Style::fg(colours::PURPLE));
        assert_eq!(style_at(&s, 21), Style::fg(colours::COMMENT));
    }

    #[test]
    fn object_keys_look_like_strings() {
        let s = highlight("{ a: b, c }");
        assert_eq!(style_at(&s, 2), Style::fg(colours::YELLOW));
        assert_eq!(style_at(&s, 5), Style::fg(colours::FOREGROUND));
        assert_eq!(style_at(&s, 8), Style::fg(colours::YELLOW));
    }

    #[test]
    fn keeps_unparseable_text() {
        assert_eq!(highlight("foo (( ] \"bar").forget_styling(), "foo (( ] \"bar");
    }
}
