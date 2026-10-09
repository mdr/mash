//! Hand-written lexer, a port of `MashLexer` + `NormalMashLexer` + `StringInterpolationLexer` + `MishLexer`.
//!
//! The lexer is always "forgiving": it never fails, it emits `ERROR_TOKEN`s and records errors instead.
//! Every emitted token is non-empty, and the concatenation of all token texts is exactly the input.

use crate::{SyntaxError, SyntaxKind, SyntaxKind::*};
use rowan::{TextRange, TextSize};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub kind: SyntaxKind,
    pub range: TextRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LexMode {
    Normal { brace_level: u32 },
    Mish,
    StringInterpolation,
    StringInterpolationIdentifier,
}

#[derive(Debug)]
pub struct LexResult {
    pub tokens: Vec<Token>,
    pub errors: Vec<SyntaxError>,
}

pub fn tokenise(text: &str, mish: bool) -> LexResult {
    let initial = if mish {
        LexMode::Mish
    } else {
        LexMode::Normal { brace_level: 0 }
    };
    let mut lexer = Lexer {
        src: text,
        pos: 0,
        start: 0,
        modes: vec![initial],
        prev: None,
        tokens: vec![],
        errors: vec![],
    };
    lexer.run();
    LexResult {
        tokens: lexer.tokens,
        errors: lexer.errors,
    }
}

const RESERVED_MISH_PUNCTUATION: &str = "$\"'#}<>";

fn keyword(s: &str) -> Option<SyntaxKind> {
    Some(match s {
        "and" => AND_KW,
        "class" => CLASS_KW,
        "def" => DEF_KW,
        "else" => ELSE_KW,
        "false" => FALSE_KW,
        "if" => IF_KW,
        "import" => IMPORT_KW,
        "namespace" => NAMESPACE_KW,
        "null" => NULL_KW,
        "or" => OR_KW,
        "then" => THEN_KW,
        "this" => THIS_KW,
        "true" => TRUE_KW,
        _ => return None,
    })
}

/// Whether `s` can be used as a bare identifier: ASCII letters, digits and `_`, not starting with a digit,
/// and not a keyword or a hole (`_`, `_1`..`_9`).
pub fn is_legal_identifier(s: &str) -> bool {
    let mut chars = s.chars();
    is_ident_start(chars.next()) && chars.all(|c| is_ident_part(Some(c))) && keyword(s).is_none() && !is_hole(s)
}

fn is_hole(s: &str) -> bool {
    let b = s.as_bytes();
    b == b"_" || (b.len() == 2 && b[0] == b'_' && (b'1'..=b'9').contains(&b[1]))
}

fn is_ident_start(c: Option<char>) -> bool {
    matches!(c, Some(c) if c.is_ascii_alphabetic() || c == '_')
}

fn is_ident_part(c: Option<char>) -> bool {
    matches!(c, Some(c) if c.is_ascii_alphanumeric() || c == '_')
}

fn is_digit(c: Option<char>) -> bool {
    matches!(c, Some(c) if c.is_ascii_digit())
}

fn is_whitespace(c: Option<char>) -> bool {
    matches!(c, Some(' ' | '\t' | '\n' | '\r'))
}

/// Tokens after which `(` and `[` mean invocation/lookup rather than grouping/list
fn is_lookup_start(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        IDENTIFIER | HOLE | RPAREN | RSQUARE | RBRACE | STRING_LITERAL | NUMBER_LITERAL | NULL_KW | THIS_KW
    )
}

struct Lexer<'a> {
    src: &'a str,
    pos: usize,
    start: usize,
    modes: Vec<LexMode>,
    /// Previous token emitted, including trivia (matters for `f(x)` vs `f (x)`)
    prev: Option<SyntaxKind>,
    tokens: Vec<Token>,
    errors: Vec<SyntaxError>,
}

impl<'a> Lexer<'a> {
    fn ch(&self) -> Option<char> {
        self.src[self.pos..].chars().next()
    }

    fn ch_at(&self, n: usize) -> Option<char> {
        self.src[self.pos..].chars().nth(n)
    }

    fn bump(&mut self) {
        if let Some(c) = self.ch() {
            self.pos += c.len_utf8();
        }
    }

    fn at_eof(&self) -> bool {
        self.pos >= self.src.len()
    }

    fn range(&self) -> TextRange {
        TextRange::new(TextSize::from(self.start as u32), TextSize::from(self.pos as u32))
    }

    fn error(&mut self, message: impl Into<String>) {
        let range = self.range();
        self.errors.push(SyntaxError {
            message: message.into(),
            range,
        });
    }

    fn emit(&mut self, kind: SyntaxKind) {
        debug_assert!(self.pos > self.start, "empty token {kind:?}");
        if self.pos > self.start {
            self.tokens.push(Token {
                kind,
                range: self.range(),
            });
            self.prev = Some(kind);
        }
        self.start = self.pos;
    }

    fn text(&self) -> &str {
        &self.src[self.start..self.pos]
    }

    fn mode(&self) -> LexMode {
        *self.modes.last().unwrap()
    }

    fn pop_mode(&mut self) {
        if self.modes.len() > 1 {
            self.modes.pop();
        }
    }

    fn run(&mut self) {
        while !self.at_eof() {
            let before = self.pos;
            match self.mode() {
                LexMode::Normal { .. } => self.normal_token(),
                LexMode::Mish => self.mish_token(),
                LexMode::StringInterpolation => self.string_interpolation_token(),
                LexMode::StringInterpolationIdentifier => self.string_interpolation_identifier_token(),
            }
            // Defensive: guarantee progress so we can never loop forever
            if self.pos == before && self.mode() == LexMode::StringInterpolationIdentifier {
                self.pop_mode();
            } else if self.pos == before {
                self.bump();
                self.emit(ERROR_TOKEN);
            }
        }
    }

    // ---- Normal mode ----

    fn normal_token(&mut self) {
        let c = self.ch().unwrap();
        match c {
            ' ' | '\t' | '\n' | '\r' => self.whitespace(),
            '0'..='9' => self.number(),
            '"' | '\'' => {
                self.bump();
                self.string_literal_rest(c)
            }
            '|' => self.single(PIPE),
            '(' => {
                self.bump();
                let kind = if self.prev.is_some_and(is_lookup_start) {
                    LPAREN_INVOKE
                } else {
                    LPAREN
                };
                self.emit(kind)
            }
            ')' => self.single(RPAREN),
            '+' => self.op_equals(PLUS, PLUS_EQUALS),
            '*' => self.op_equals(TIMES, TIMES_EQUALS),
            '/' => self.op_equals(DIVIDE, DIVIDE_EQUALS),
            '~' => self.single(TILDE),
            ';' => self.single(SEMI),
            '.' => {
                self.bump();
                if self.ch() == Some('.') && self.ch_at(1) == Some('.') {
                    self.bump();
                    self.bump();
                    self.emit(ELLIPSIS)
                } else {
                    self.emit(DOT)
                }
            }
            '?' => {
                self.bump();
                if self.ch() == Some('.') {
                    self.bump();
                    self.emit(DOT_NULL_SAFE)
                } else {
                    self.emit(QUESTION)
                }
            }
            '!' => self.bang(),
            '-' => {
                self.bump();
                match self.ch() {
                    Some('-') => {
                        self.bump();
                        if matches!(self.ch(), Some(c) if c.is_alphabetic()) {
                            self.ident_rest(Some(LONG_FLAG))
                        } else {
                            self.error("Incomplete flag");
                            self.emit(LONG_FLAG)
                        }
                    }
                    Some('=') => {
                        self.bump();
                        self.emit(MINUS_EQUALS)
                    }
                    c if is_digit(c) => self.number(),
                    Some(c) if c.is_alphabetic() => self.ident_rest(Some(SHORT_FLAG)),
                    _ => self.emit(MINUS),
                }
            }
            '>' => self.op_equals(GREATER_THAN, GREATER_THAN_EQUALS),
            '<' => self.op_equals(LESS_THAN, LESS_THAN_EQUALS),
            '=' => {
                self.bump();
                match self.ch() {
                    Some('=') => {
                        self.bump();
                        self.emit(LONG_EQUALS)
                    }
                    Some('>') => {
                        self.bump();
                        self.emit(RIGHT_ARROW)
                    }
                    _ => self.emit(SHORT_EQUALS),
                }
            }
            '#' => self.comment(),
            c if c.is_ascii_alphabetic() || c == '_' => {
                self.bump();
                self.ident_rest(None)
            }
            '[' => {
                self.bump();
                let kind = if self.prev.is_some_and(is_lookup_start) {
                    LSQUARE_LOOKUP
                } else {
                    LSQUARE
                };
                self.emit(kind)
            }
            '{' => {
                if let Some(LexMode::Normal { brace_level }) = self.modes.last_mut() {
                    *brace_level += 1;
                }
                self.single(LBRACE)
            }
            '}' => {
                match self.modes.last_mut() {
                    Some(LexMode::Normal { brace_level }) if *brace_level > 0 => *brace_level -= 1,
                    _ => self.pop_mode(),
                }
                self.single(RBRACE)
            }
            ']' => self.single(RSQUARE),
            ',' => self.single(COMMA),
            ':' => self.single(COLON),
            '@' => self.single(AT),
            _ => {
                self.bump();
                self.error(format!("Unexpected character: {c}"));
                self.emit(ERROR_TOKEN)
            }
        }
    }

    fn single(&mut self, kind: SyntaxKind) {
        self.bump();
        self.emit(kind)
    }

    fn op_equals(&mut self, plain: SyntaxKind, with_equals: SyntaxKind) {
        self.bump();
        if self.ch() == Some('=') {
            self.bump();
            self.emit(with_equals)
        } else {
            self.emit(plain)
        }
    }

    fn bang(&mut self) {
        self.bump(); // !
        match self.ch() {
            Some('=') => {
                self.bump();
                self.emit(NOT_EQUALS)
            }
            Some('!') => {
                self.bump();
                if self.ch() == Some('{') {
                    self.bump();
                    self.modes.push(LexMode::Mish);
                    self.emit(MISH_INTERPOLATION_START_NO_CAPTURE)
                } else {
                    self.bump();
                    self.error("Unexpected character after '!!'");
                    self.emit(ERROR_TOKEN)
                }
            }
            Some('{') => {
                self.bump();
                self.modes.push(LexMode::Mish);
                self.emit(MISH_INTERPOLATION_START)
            }
            c if self.is_mish_word_char(c) => self.mish_word_rest(),
            _ => {
                self.bump();
                self.error("Unexpected character after '!'");
                self.emit(ERROR_TOKEN)
            }
        }
    }

    fn whitespace(&mut self) {
        while is_whitespace(self.ch()) {
            self.bump();
        }
        self.emit(WHITESPACE)
    }

    fn comment(&mut self) {
        self.bump(); // #
        loop {
            match self.ch() {
                None => break,
                Some('\n') => {
                    self.bump();
                    break;
                }
                Some('\r') if self.ch_at(1) != Some('\n') => {
                    self.bump();
                    break;
                }
                _ => self.bump(),
            }
        }
        self.emit(COMMENT)
    }

    fn ident_rest(&mut self, kind: Option<SyntaxKind>) {
        while is_ident_part(self.ch()) {
            self.bump();
        }
        let kind = kind.unwrap_or_else(|| {
            let text = self.text();
            if is_hole(text) {
                HOLE
            } else {
                keyword(text).unwrap_or(IDENTIFIER)
            }
        });
        self.emit(kind)
    }

    /// Called with the current char being the first digit (any '-' already consumed)
    fn number(&mut self) {
        if self.ch() == Some('0') {
            self.bump();
            if self.ch() == Some('.') && is_digit(self.ch_at(1)) {
                self.bump();
                self.fraction();
                return;
            } else if is_digit(self.ch()) {
                self.error("Invalid number literal");
            } else {
                self.emit(NUMBER_LITERAL);
                return;
            }
        }
        while is_digit(self.ch()) {
            self.bump();
        }
        if self.ch() == Some('.') && is_digit(self.ch_at(1)) {
            self.bump();
            self.fraction();
        } else {
            self.emit(NUMBER_LITERAL)
        }
    }

    fn fraction(&mut self) {
        while is_digit(self.ch()) {
            self.bump();
        }
        self.emit(NUMBER_LITERAL)
    }

    /// After the opening quote. May switch into string interpolation mode (for `"` strings).
    fn string_literal_rest(&mut self, delimiter: char) {
        loop {
            match self.ch() {
                Some('`') => {
                    self.bump();
                    match self.ch() {
                        Some('`' | '$' | 'n' | 'r' | 't' | '"' | '\'' | '~') => self.bump(),
                        Some(c) => {
                            self.bump();
                            self.error(format!("Invalid string escape `{c}"));
                        }
                        None => {}
                    }
                }
                Some(c) if c == delimiter => {
                    self.bump();
                    return self.emit(STRING_LITERAL);
                }
                None => {
                    self.error("Unterminated string literal");
                    return self.emit(STRING_LITERAL);
                }
                Some('$') if delimiter == '"' => {
                    // Don't consume the dollar; switch mode
                    self.modes.push(LexMode::StringInterpolation);
                    return self.emit(STRING_START);
                }
                Some(_) => self.bump(),
            }
        }
    }

    // ---- String interpolation modes ----

    fn string_interpolation_token(&mut self) {
        if self.ch() == Some('$') {
            self.dollar()
        } else {
            self.interpolated_string_part()
        }
    }

    /// `$` or `${`. Used both inside strings and in mish mode.
    fn dollar(&mut self) {
        self.bump(); // $
        if self.ch() == Some('{') {
            self.bump();
            self.modes.push(LexMode::Normal { brace_level: 0 });
            self.emit(STRING_INTERPOLATION_START_COMPLEX)
        } else if is_ident_start(self.ch()) {
            self.modes.push(LexMode::StringInterpolationIdentifier);
            self.emit(STRING_INTERPOLATION_START_SIMPLE)
        } else {
            self.error("Invalid string interpolation sequence");
            self.emit(STRING_INTERPOLATION_START_SIMPLE)
        }
    }

    fn interpolated_string_part(&mut self) {
        loop {
            match self.ch() {
                Some('`') => {
                    self.bump();
                    match self.ch() {
                        Some('`' | '$' | 'n' | 'r' | 't' | '"' | '\'' | '~') => self.bump(),
                        Some(c) => {
                            self.bump();
                            self.error(format!("Invalid string escape `{c}"));
                        }
                        None => {}
                    }
                }
                Some('"') => {
                    self.bump();
                    return self.end_string();
                }
                None => {
                    self.error("Unterminated string literal");
                    return self.end_string();
                }
                Some('$') => return self.emit(STRING_MIDDLE),
                Some(_) => self.bump(),
            }
        }
    }

    fn end_string(&mut self) {
        // Only pop if we're interpolating a string, not a mish fragment
        if self.mode() == LexMode::StringInterpolation {
            self.modes.pop();
        }
        if self.pos > self.start {
            self.emit(STRING_END)
        }
    }

    fn string_interpolation_identifier_token(&mut self) {
        if is_ident_start(self.ch()) {
            self.bump();
            self.ident_rest(None)
        } else if self.ch() == Some('.') {
            self.single(DOT)
        } else {
            self.pop_mode();
            return;
        }
        if !is_ident_start(self.ch()) && self.ch() != Some('.') {
            self.pop_mode();
        }
    }

    // ---- Mish mode ----

    fn is_mish_word_char(&self, c: Option<char>) -> bool {
        matches!(c, Some(c) if !c.is_whitespace() && !RESERVED_MISH_PUNCTUATION.contains(c))
    }

    fn mish_word_rest(&mut self) {
        while self.is_mish_word_char(self.ch()) {
            self.bump();
        }
        self.emit(MISH_WORD)
    }

    fn mish_token(&mut self) {
        let c = self.ch();
        match c {
            c if self.is_mish_word_char(c) => {
                self.bump();
                self.mish_word_rest()
            }
            Some(' ' | '\t' | '\n' | '\r') => self.whitespace(),
            Some(c) if c.is_whitespace() => self.whitespace_unicode(),
            Some('$') => self.dollar(),
            Some('#') => self.comment(),
            Some('>') => self.single(GREATER_THAN),
            Some('<') => self.single(LESS_THAN),
            Some('}') => {
                self.pop_mode();
                self.single(RBRACE)
            }
            Some(q @ ('"' | '\'')) => {
                self.bump();
                self.string_literal_rest(q)
            }
            _ => {
                self.bump();
                self.error("Unexpected character");
                self.emit(ERROR_TOKEN)
            }
        }
    }

    /// Non-ASCII whitespace in mish mode isn't a word char; treat it as an error token (Scala does too)
    fn whitespace_unicode(&mut self) {
        self.bump();
        self.error("Unexpected whitespace character");
        self.emit(ERROR_TOKEN)
    }
}
