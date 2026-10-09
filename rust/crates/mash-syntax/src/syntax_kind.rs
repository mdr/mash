//! Token and node kinds. Token kinds mirror the Scala `TokenType`s; node kinds mirror `ConcreteSyntax`.

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u16)]
pub enum SyntaxKind {
    // ---- Trivia ----
    WHITESPACE = 0,
    COMMENT,

    // ---- Tokens ----
    IDENTIFIER,
    LONG_FLAG,           // --flag
    SHORT_FLAG,          // -f
    SHORT_EQUALS,        // =
    LONG_EQUALS,         // ==
    NOT_EQUALS,          // !=
    GREATER_THAN_EQUALS, // >=
    GREATER_THAN,        // >
    LESS_THAN_EQUALS,    // <=
    LESS_THAN,           // <
    RIGHT_ARROW,         // =>
    PLUS,
    MINUS,
    TIMES,
    DIVIDE,
    PLUS_EQUALS,
    MINUS_EQUALS,
    TIMES_EQUALS,
    DIVIDE_EQUALS,
    PIPE,
    LPAREN,
    LPAREN_INVOKE, // foo(
    RPAREN,
    LBRACE,
    RBRACE,
    LSQUARE,
    LSQUARE_LOOKUP, // foo[
    RSQUARE,
    COMMA,
    COLON,
    DOT,
    DOT_NULL_SAFE, // ?.
    ELLIPSIS,      // ...
    HOLE,          // _ or _1.._9
    TRUE_KW,
    FALSE_KW,
    STRING_LITERAL,
    NUMBER_LITERAL,
    AND_KW,
    OR_KW,
    IF_KW,
    THEN_KW,
    ELSE_KW,
    SEMI,
    NULL_KW,
    TILDE,
    STRING_START,                       // "foo  (up to an interpolation)
    STRING_MIDDLE,                      // bar   (between interpolations)
    STRING_END,                         // baz"
    STRING_INTERPOLATION_START_COMPLEX, // ${
    STRING_INTERPOLATION_START_SIMPLE,  // $
    MISH_WORD,
    MISH_INTERPOLATION_START,            // !{
    MISH_INTERPOLATION_START_NO_CAPTURE, // !!{
    DEF_KW,
    QUESTION,
    CLASS_KW,
    THIS_KW,
    NAMESPACE_KW,
    AT,
    IMPORT_KW,
    /// A character the lexer couldn't make sense of
    ERROR_TOKEN,

    // ---- Nodes ----
    SOURCE_FILE,
    NAMESPACE_DECL,
    IMPORT_STMT,
    STATEMENT_SEQ,

    LITERAL,
    INTERPOLATED_STRING,
    SIMPLE_INTERPOLATION,
    COMPLEX_INTERPOLATION,
    IDENTIFIER_EXPR,
    HOLE_EXPR,
    THIS_EXPR,
    MISH_FUNCTION,
    PAREN_EXPR,
    LIST_EXPR,
    OBJECT_EXPR,
    FULL_OBJECT_ENTRY,
    SHORTHAND_OBJECT_ENTRY,
    BLOCK_EXPR,
    MEMBER_EXPR,
    HEADLESS_MEMBER_EXPR,
    LOOKUP_EXPR,
    PAREN_INVOCATION_EXPR,
    HELP_EXPR,
    INVOCATION_EXPR,
    LONG_ARG,
    SHORT_ARG,
    MINUS_EXPR,
    BIN_OP_EXPR,
    CHAINED_OP_EXPR,
    IF_EXPR,
    ASSIGNMENT_EXPR,
    PATTERN_ASSIGNMENT_EXPR,
    LAMBDA_EXPR,
    PIPE_EXPR,

    PARAM_LIST,
    SIMPLE_PARAM,
    PAREN_PARAM,
    IDENT_PATTERN,
    HOLE_PATTERN,
    OBJECT_PATTERN,
    FULL_OBJECT_PATTERN_ENTRY,
    SHORTHAND_OBJECT_PATTERN_ENTRY,
    LIST_PATTERN,

    FUNCTION_DECL,
    ATTRIBUTES,
    ATTRIBUTE,
    CLASS_DECL,
    CLASS_BODY,

    MISH_INTERPOLATION_EXPR,
    MISH_EXPR,
    MISH_REDIRECT,

    /// Tokens the parser couldn't fit anywhere
    ERROR,

    #[doc(hidden)]
    __LAST,
}

use SyntaxKind::*;

impl SyntaxKind {
    pub fn is_trivia(self) -> bool {
        matches!(self, WHITESPACE | COMMENT)
    }

    pub fn is_keyword(self) -> bool {
        matches!(
            self,
            TRUE_KW
                | FALSE_KW
                | AND_KW
                | OR_KW
                | IF_KW
                | THEN_KW
                | ELSE_KW
                | NULL_KW
                | DEF_KW
                | CLASS_KW
                | THIS_KW
                | NAMESPACE_KW
                | IMPORT_KW
        )
    }

    pub fn is_token(self) -> bool {
        (self as u16) <= (ERROR_TOKEN as u16)
    }

    /// Node kinds that are expressions (as used by the typed AST layer).
    pub fn is_expr(self) -> bool {
        matches!(
            self,
            STATEMENT_SEQ
                | LITERAL
                | INTERPOLATED_STRING
                | IDENTIFIER_EXPR
                | HOLE_EXPR
                | THIS_EXPR
                | MISH_FUNCTION
                | PAREN_EXPR
                | LIST_EXPR
                | OBJECT_EXPR
                | BLOCK_EXPR
                | MEMBER_EXPR
                | HEADLESS_MEMBER_EXPR
                | LOOKUP_EXPR
                | PAREN_INVOCATION_EXPR
                | HELP_EXPR
                | INVOCATION_EXPR
                | MINUS_EXPR
                | BIN_OP_EXPR
                | CHAINED_OP_EXPR
                | IF_EXPR
                | ASSIGNMENT_EXPR
                | PATTERN_ASSIGNMENT_EXPR
                | LAMBDA_EXPR
                | PIPE_EXPR
                | FUNCTION_DECL
                | CLASS_DECL
                | IMPORT_STMT
                | MISH_INTERPOLATION_EXPR
        )
    }

    /// Kinds that open a bracketed group (for lookahead scans).
    pub(crate) fn is_opener(self) -> bool {
        matches!(
            self,
            LPAREN
                | LPAREN_INVOKE
                | LBRACE
                | LSQUARE
                | LSQUARE_LOOKUP
                | STRING_INTERPOLATION_START_COMPLEX
                | MISH_INTERPOLATION_START
                | MISH_INTERPOLATION_START_NO_CAPTURE
        )
    }

    pub(crate) fn is_closer(self) -> bool {
        matches!(self, RPAREN | RBRACE | RSQUARE)
    }

    /// Human-friendly description for error messages.
    pub fn describe(self) -> &'static str {
        match self {
            IDENTIFIER => "identifier",
            RBRACE => "}",
            RPAREN => ")",
            RSQUARE => "]",
            COLON => ":",
            DOT => ".",
            THEN_KW => "then",
            SHORT_EQUALS => "=",
            RIGHT_ARROW => "=>",
            STRING_END => "end of string",
            _ => "token",
        }
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        rowan::SyntaxKind(kind as u16)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MashLanguage {}

impl rowan::Language for MashLanguage {
    type Kind = SyntaxKind;

    #[allow(unsafe_code)]
    fn kind_from_raw(raw: rowan::SyntaxKind) -> SyntaxKind {
        assert!(raw.0 < __LAST as u16);
        // SAFETY: SyntaxKind is repr(u16) and raw.0 is in range (checked above)
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }

    fn kind_to_raw(kind: SyntaxKind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<MashLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<MashLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<MashLanguage>;
pub type SyntaxNodePtr = rowan::ast::SyntaxNodePtr<MashLanguage>;
