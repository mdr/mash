//! Typed wrappers over the untyped rowan tree (rust-analyzer style). Every accessor returns `Option`
//! (or a possibly-empty collection) because the tree may be incomplete.
//!
//! Wrappers are hand-written here; for the real implementation they could be generated from a grammar file.

use crate::doc_comment::DocComment;
use crate::{MashLanguage, SyntaxKind, SyntaxKind::*, SyntaxNode, SyntaxToken};
use rowan::NodeOrToken;
pub use rowan::ast::AstNode;

macro_rules! ast_node {
    ($($name:ident => $kind:ident),* $(,)?) => {$(
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $name(SyntaxNode);

        impl AstNode for $name {
            type Language = MashLanguage;
            fn can_cast(kind: SyntaxKind) -> bool { kind == $kind }
            fn cast(node: SyntaxNode) -> Option<Self> {
                if Self::can_cast(node.kind()) { Some(Self(node)) } else { None }
            }
            fn syntax(&self) -> &SyntaxNode { &self.0 }
        }
    )*};
}

ast_node! {
    SourceFile => SOURCE_FILE,
    NamespaceDecl => NAMESPACE_DECL,
    ImportStmt => IMPORT_STMT,
    StatementSeq => STATEMENT_SEQ,
    Literal => LITERAL,
    InterpolatedString => INTERPOLATED_STRING,
    SimpleInterpolation => SIMPLE_INTERPOLATION,
    ComplexInterpolation => COMPLEX_INTERPOLATION,
    IdentifierExpr => IDENTIFIER_EXPR,
    HoleExpr => HOLE_EXPR,
    ThisExpr => THIS_EXPR,
    MishFunction => MISH_FUNCTION,
    ParenExpr => PAREN_EXPR,
    ListExpr => LIST_EXPR,
    ObjectExpr => OBJECT_EXPR,
    FullObjectEntry => FULL_OBJECT_ENTRY,
    ShorthandObjectEntry => SHORTHAND_OBJECT_ENTRY,
    BlockExpr => BLOCK_EXPR,
    MemberExpr => MEMBER_EXPR,
    HeadlessMemberExpr => HEADLESS_MEMBER_EXPR,
    LookupExpr => LOOKUP_EXPR,
    ParenInvocationExpr => PAREN_INVOCATION_EXPR,
    HelpExpr => HELP_EXPR,
    InvocationExpr => INVOCATION_EXPR,
    LongArg => LONG_ARG,
    ShortArg => SHORT_ARG,
    MinusExpr => MINUS_EXPR,
    BinOpExpr => BIN_OP_EXPR,
    ChainedOpExpr => CHAINED_OP_EXPR,
    IfExpr => IF_EXPR,
    AssignmentExpr => ASSIGNMENT_EXPR,
    PatternAssignmentExpr => PATTERN_ASSIGNMENT_EXPR,
    LambdaExpr => LAMBDA_EXPR,
    PipeExpr => PIPE_EXPR,
    ParamList => PARAM_LIST,
    SimpleParam => SIMPLE_PARAM,
    ParenParam => PAREN_PARAM,
    IdentPattern => IDENT_PATTERN,
    HolePattern => HOLE_PATTERN,
    ObjectPattern => OBJECT_PATTERN,
    FullObjectPatternEntry => FULL_OBJECT_PATTERN_ENTRY,
    ShorthandObjectPatternEntry => SHORTHAND_OBJECT_PATTERN_ENTRY,
    ListPattern => LIST_PATTERN,
    FunctionDecl => FUNCTION_DECL,
    Attributes => ATTRIBUTES,
    Attribute => ATTRIBUTE,
    ClassDecl => CLASS_DECL,
    ClassBody => CLASS_BODY,
    MishInterpolationExpr => MISH_INTERPOLATION_EXPR,
    MishExpr => MISH_EXPR,
    MishRedirect => MISH_REDIRECT,
}

// ------------------------------------------------------------------------------------------------ enums

macro_rules! ast_enum {
    ($enum:ident { $($variant:ident($ty:ident)),* $(,)? }) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum $enum { $($variant($ty)),* }

        impl AstNode for $enum {
            type Language = MashLanguage;
            fn can_cast(kind: SyntaxKind) -> bool { $( $ty::can_cast(kind) )||* }
            fn cast(node: SyntaxNode) -> Option<Self> {
                $( if $ty::can_cast(node.kind()) { return Some($enum::$variant($ty(node))); } )*
                None
            }
            fn syntax(&self) -> &SyntaxNode {
                match self { $( $enum::$variant(n) => n.syntax() ),* }
            }
        }
    };
}

ast_enum!(Expr {
    StatementSeq(StatementSeq),
    Literal(Literal),
    InterpolatedString(InterpolatedString),
    Identifier(IdentifierExpr),
    Hole(HoleExpr),
    This(ThisExpr),
    MishFunction(MishFunction),
    Paren(ParenExpr),
    List(ListExpr),
    Object(ObjectExpr),
    Block(BlockExpr),
    Member(MemberExpr),
    HeadlessMember(HeadlessMemberExpr),
    Lookup(LookupExpr),
    ParenInvocation(ParenInvocationExpr),
    Help(HelpExpr),
    Invocation(InvocationExpr),
    Minus(MinusExpr),
    BinOp(BinOpExpr),
    ChainedOp(ChainedOpExpr),
    If(IfExpr),
    Assignment(AssignmentExpr),
    PatternAssignment(PatternAssignmentExpr),
    Lambda(LambdaExpr),
    Pipe(PipeExpr),
    FunctionDecl(FunctionDecl),
    ClassDecl(ClassDecl),
    Import(ImportStmt),
    MishInterpolation(MishInterpolationExpr),
});

ast_enum!(Pattern {
    Ident(IdentPattern),
    Hole(HolePattern),
    Object(ObjectPattern),
    List(ListPattern),
});

ast_enum!(Param {
    Simple(SimpleParam),
    Paren(ParenParam),
});

ast_enum!(ObjectEntry {
    Full(FullObjectEntry),
    Shorthand(ShorthandObjectEntry),
});

ast_enum!(ObjectPatternEntry {
    Full(FullObjectPatternEntry),
    Shorthand(ShorthandObjectPatternEntry),
});

/// An argument in a juxtaposition invocation `f a --b=c -d`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Arg {
    Positional(Expr),
    Short(ShortArg),
    Long(LongArg),
}

// ------------------------------------------------------------------------------------------------ helpers

fn tokens(node: &SyntaxNode) -> impl Iterator<Item = SyntaxToken> {
    node.children_with_tokens()
        .filter_map(|e| e.into_token())
        .filter(|t| !t.kind().is_trivia())
}

fn token(node: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
    tokens(node).find(|t| t.kind() == kind)
}

fn child<N: AstNode<Language = MashLanguage>>(node: &SyntaxNode) -> Option<N> {
    node.children().find_map(N::cast)
}

fn children<N: AstNode<Language = MashLanguage>>(node: &SyntaxNode) -> Vec<N> {
    node.children().filter_map(N::cast).collect()
}

/// First child of type N appearing after a (direct child) token of one of the given kinds
fn child_after<N: AstNode<Language = MashLanguage>>(node: &SyntaxNode, kinds: &[SyntaxKind]) -> Option<N> {
    let mut seen = false;
    for element in node.children_with_tokens() {
        match element {
            NodeOrToken::Token(t) if kinds.contains(&t.kind()) => seen = true,
            NodeOrToken::Node(n) if seen => {
                if let Some(n) = N::cast(n) {
                    return Some(n);
                }
            }
            _ => {}
        }
    }
    None
}

/// First child of type N appearing before any (direct child) token of one of the given kinds
fn child_before<N: AstNode<Language = MashLanguage>>(node: &SyntaxNode, kinds: &[SyntaxKind]) -> Option<N> {
    for element in node.children_with_tokens() {
        match element {
            NodeOrToken::Token(t) if kinds.contains(&t.kind()) => return None,
            NodeOrToken::Node(n) => {
                if let Some(n) = N::cast(n) {
                    return Some(n);
                }
            }
            _ => {}
        }
    }
    None
}

// ------------------------------------------------------------------------------------------------ strings

/// Port of Abstractifier.getStringText: strip quotes, detect a `~` prefix, unescape. Returns (text, has_tilde).
pub fn string_text(raw: &str, maybe_tilde: bool, prune_initial: bool, prune_final: bool) -> (String, bool) {
    let mut s = raw;
    if prune_final && (s.ends_with('"') || s.ends_with('\'')) && s.len() >= 2 {
        s = &s[..s.len() - 1];
    }
    if prune_initial && (s.starts_with('"') || s.starts_with('\'')) {
        s = &s[1..];
    }
    let tilde = maybe_tilde && s.starts_with('~');
    if tilde {
        s = &s[1..];
    }
    (crate::escapes::unescape(s), tilde)
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Number(f64),
    String {
        value: String,
        double_quoted: bool,
        tilde: bool,
    },
    Bool(bool),
    Null,
}

impl Literal {
    pub fn token(&self) -> Option<SyntaxToken> {
        tokens(&self.0).next()
    }

    pub fn value(&self) -> Option<LiteralValue> {
        let t = self.token()?;
        Some(match t.kind() {
            NUMBER_LITERAL => LiteralValue::Number(t.text().parse().ok()?),
            TRUE_KW => LiteralValue::Bool(true),
            FALSE_KW => LiteralValue::Bool(false),
            NULL_KW => LiteralValue::Null,
            STRING_LITERAL => {
                let text = t.text();
                let (value, tilde) = string_text(text, true, true, true);
                LiteralValue::String {
                    value,
                    double_quoted: text.starts_with('"'),
                    tilde,
                }
            }
            _ => return None,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InterpolationPart {
    /// Literal text (already unescaped, quotes stripped)
    Text(String),
    Simple(SimpleInterpolation),
    Complex(ComplexInterpolation),
}

impl InterpolatedString {
    /// Whether the string starts with `~` (home directory prefix)
    pub fn tilde(&self) -> bool {
        tokens(&self.0)
            .next()
            .is_some_and(|t| t.kind() == STRING_START && string_text(t.text(), true, true, false).1)
    }

    pub fn parts(&self) -> Vec<InterpolationPart> {
        let mut parts = vec![];
        for element in self.0.children_with_tokens() {
            match element {
                NodeOrToken::Token(t) => {
                    let text = match t.kind() {
                        STRING_START => string_text(t.text(), true, true, false).0,
                        STRING_MIDDLE => string_text(t.text(), false, false, false).0,
                        STRING_END => string_text(t.text(), false, false, true).0,
                        _ => continue,
                    };
                    if !text.is_empty() {
                        parts.push(InterpolationPart::Text(text));
                    }
                }
                NodeOrToken::Node(n) => {
                    if let Some(s) = SimpleInterpolation::cast(n.clone()) {
                        parts.push(InterpolationPart::Simple(s));
                    } else if let Some(c) = ComplexInterpolation::cast(n) {
                        parts.push(InterpolationPart::Complex(c));
                    }
                }
            }
        }
        parts
    }
}

impl SimpleInterpolation {
    pub fn expr(&self) -> Option<Expr> {
        child(&self.0)
    }
}

impl ComplexInterpolation {
    pub fn expr(&self) -> Option<Expr> {
        child(&self.0)
    }
}

// ------------------------------------------------------------------------------------------------ simple exprs

impl SourceFile {
    pub fn namespace(&self) -> Option<NamespaceDecl> {
        child(&self.0)
    }

    /// Top-level expressions. Normally exactly one (possibly a StatementSeq); more only after error recovery.
    pub fn exprs(&self) -> Vec<Expr> {
        children(&self.0)
    }

    /// For mish-mode parses
    pub fn mish(&self) -> Option<MishExpr> {
        child(&self.0)
    }
}

impl NamespaceDecl {
    pub fn segments(&self) -> Vec<String> {
        tokens(&self.0)
            .filter(|t| t.kind() == IDENTIFIER)
            .map(|t| t.text().to_string())
            .collect()
    }
}

impl ImportStmt {
    /// e.g. `import a.b._` => ["a", "b", "_"]
    pub fn segments(&self) -> Vec<String> {
        tokens(&self.0)
            .filter(|t| matches!(t.kind(), IDENTIFIER | HOLE))
            .map(|t| t.text().to_string())
            .collect()
    }
}

impl StatementSeq {
    pub fn statements(&self) -> Vec<Expr> {
        children(&self.0)
    }
}

impl IdentifierExpr {
    pub fn name_token(&self) -> Option<SyntaxToken> {
        token(&self.0, IDENTIFIER)
    }
    pub fn name(&self) -> String {
        self.name_token().map(|t| t.text().to_string()).unwrap_or_default()
    }
}

impl HoleExpr {
    /// None for `_`, Some(n) for `_n`
    pub fn index(&self) -> Option<u8> {
        let t = token(&self.0, HOLE)?;
        t.text().strip_prefix('_')?.parse().ok()
    }
}

impl MishFunction {
    /// The command name, without the leading `!`
    pub fn command(&self) -> String {
        tokens(&self.0)
            .next()
            .map(|t| t.text().trim_start_matches('!').to_string())
            .unwrap_or_default()
    }
}

impl ParenExpr {
    pub fn expr(&self) -> Option<Expr> {
        child(&self.0)
    }
}

impl ListExpr {
    pub fn items(&self) -> Vec<Expr> {
        children(&self.0)
    }
}

impl ObjectExpr {
    pub fn entries(&self) -> Vec<ObjectEntry> {
        children(&self.0)
    }
}

impl FullObjectEntry {
    pub fn field(&self) -> Option<Expr> {
        child_before(&self.0, &[COLON])
    }
    pub fn value(&self) -> Option<Expr> {
        child_after(&self.0, &[COLON])
    }
}

impl ShorthandObjectEntry {
    pub fn name(&self) -> String {
        token(&self.0, IDENTIFIER)
            .map(|t| t.text().to_string())
            .unwrap_or_default()
    }
}

impl BlockExpr {
    pub fn body(&self) -> Option<Expr> {
        child(&self.0)
    }
}

impl MemberExpr {
    pub fn target(&self) -> Option<Expr> {
        child_before(&self.0, &[DOT, DOT_NULL_SAFE])
    }
    pub fn is_null_safe(&self) -> bool {
        token(&self.0, DOT_NULL_SAFE).is_some()
    }
    pub fn dot_token(&self) -> Option<SyntaxToken> {
        tokens(&self.0).find(|t| matches!(t.kind(), DOT | DOT_NULL_SAFE))
    }
    /// The member name; None if missing (e.g. `foo.` while typing)
    pub fn name_token(&self) -> Option<SyntaxToken> {
        token(&self.0, IDENTIFIER)
    }
}

impl HeadlessMemberExpr {
    pub fn is_null_safe(&self) -> bool {
        token(&self.0, DOT_NULL_SAFE).is_some()
    }
    pub fn name_token(&self) -> Option<SyntaxToken> {
        token(&self.0, IDENTIFIER)
    }
}

impl LookupExpr {
    pub fn target(&self) -> Option<Expr> {
        child_before(&self.0, &[LSQUARE_LOOKUP])
    }
    pub fn index(&self) -> Option<Expr> {
        child_after(&self.0, &[LSQUARE_LOOKUP])
    }
}

impl ParenInvocationExpr {
    pub fn function(&self) -> Option<Expr> {
        child_before(&self.0, &[LPAREN_INVOKE])
    }
    pub fn args(&self) -> Vec<Expr> {
        let mut seen = false;
        let mut args = vec![];
        for element in self.0.children_with_tokens() {
            match element {
                NodeOrToken::Token(t) if t.kind() == LPAREN_INVOKE => seen = true,
                NodeOrToken::Node(n) if seen => args.extend(Expr::cast(n)),
                _ => {}
            }
        }
        args
    }
}

impl HelpExpr {
    pub fn expr(&self) -> Option<Expr> {
        child(&self.0)
    }
}

impl InvocationExpr {
    pub fn function(&self) -> Option<Expr> {
        self.0.children().next().and_then(Expr::cast)
    }

    pub fn args(&self) -> Vec<Arg> {
        self.0
            .children()
            .skip(1)
            .filter_map(|n| match n.kind() {
                SHORT_ARG => Some(Arg::Short(ShortArg(n))),
                LONG_ARG => Some(Arg::Long(LongArg(n))),
                _ => Expr::cast(n).map(Arg::Positional),
            })
            .collect()
    }
}

impl ShortArg {
    /// The flag characters, e.g. "abc" for `-abc`
    pub fn flags(&self) -> String {
        tokens(&self.0)
            .next()
            .map(|t| t.text().trim_start_matches('-').to_string())
            .unwrap_or_default()
    }
}

impl LongArg {
    /// The flag name without `--`
    pub fn name(&self) -> String {
        token(&self.0, LONG_FLAG)
            .map(|t| t.text().trim_start_matches('-').to_string())
            .unwrap_or_default()
    }
    pub fn value(&self) -> Option<Expr> {
        child_after(&self.0, &[SHORT_EQUALS])
    }
}

impl MinusExpr {
    pub fn operand(&self) -> Option<Expr> {
        child(&self.0)
    }
}

const BIN_OPS: &[SyntaxKind] = &[
    PLUS,
    MINUS,
    TIMES,
    DIVIDE,
    LONG_EQUALS,
    NOT_EQUALS,
    LESS_THAN,
    LESS_THAN_EQUALS,
    GREATER_THAN,
    GREATER_THAN_EQUALS,
    AND_KW,
    OR_KW,
];

impl BinOpExpr {
    pub fn lhs(&self) -> Option<Expr> {
        child_before(&self.0, BIN_OPS)
    }
    pub fn op(&self) -> Option<SyntaxToken> {
        tokens(&self.0).find(|t| BIN_OPS.contains(&t.kind()))
    }
    pub fn rhs(&self) -> Option<Expr> {
        child_after(&self.0, BIN_OPS)
    }
}

impl ChainedOpExpr {
    pub fn first(&self) -> Option<Expr> {
        child_before(&self.0, BIN_OPS)
    }
    /// (operator, operand) pairs after the first operand
    pub fn rest(&self) -> Vec<(SyntaxKind, Option<Expr>)> {
        let mut result: Vec<(SyntaxKind, Option<Expr>)> = vec![];
        for element in self.0.children_with_tokens() {
            match element {
                NodeOrToken::Token(t) if BIN_OPS.contains(&t.kind()) => result.push((t.kind(), None)),
                NodeOrToken::Node(n) => {
                    if let Some(last) = result.last_mut()
                        && last.1.is_none()
                    {
                        last.1 = Expr::cast(n);
                    }
                }
                _ => {}
            }
        }
        result
    }
}

impl IfExpr {
    pub fn condition(&self) -> Option<Expr> {
        child_before(&self.0, &[THEN_KW, ELSE_KW])
    }
    pub fn then_branch(&self) -> Option<Expr> {
        child_after(&self.0, &[THEN_KW]).filter(|e| self.else_branch().as_ref() != Some(e))
    }
    pub fn else_branch(&self) -> Option<Expr> {
        child_after(&self.0, &[ELSE_KW])
    }
}

const ASSIGN_OPS: &[SyntaxKind] = &[SHORT_EQUALS, PLUS_EQUALS, MINUS_EQUALS, TIMES_EQUALS, DIVIDE_EQUALS];

impl AssignmentExpr {
    pub fn lhs(&self) -> Option<Expr> {
        child_before(&self.0, ASSIGN_OPS)
    }
    pub fn op(&self) -> Option<SyntaxKind> {
        tokens(&self.0).map(|t| t.kind()).find(|k| ASSIGN_OPS.contains(k))
    }
    pub fn rhs(&self) -> Option<Expr> {
        child_after(&self.0, ASSIGN_OPS)
    }
}

impl PatternAssignmentExpr {
    pub fn pattern(&self) -> Option<Pattern> {
        child(&self.0)
    }
    pub fn rhs(&self) -> Option<Expr> {
        child_after(&self.0, &[SHORT_EQUALS])
    }
}

impl LambdaExpr {
    pub fn params(&self) -> Option<ParamList> {
        child(&self.0)
    }
    pub fn body(&self) -> Option<Expr> {
        child_after(&self.0, &[RIGHT_ARROW])
    }
}

impl PipeExpr {
    pub fn lhs(&self) -> Option<Expr> {
        child_before(&self.0, &[PIPE])
    }
    pub fn rhs(&self) -> Option<Expr> {
        child_after(&self.0, &[PIPE])
    }
}

// ------------------------------------------------------------------------------------------------ params & patterns

impl ParamList {
    pub fn params(&self) -> Vec<Param> {
        children(&self.0)
    }
}

impl SimpleParam {
    pub fn pattern(&self) -> Option<Pattern> {
        child(&self.0)
    }
    pub fn is_variadic(&self) -> bool {
        token(&self.0, ELLIPSIS).is_some()
    }
}

impl ParenParam {
    pub fn attributes(&self) -> Option<Attributes> {
        child(&self.0)
    }
    pub fn param(&self) -> Option<SimpleParam> {
        child(&self.0)
    }
    pub fn default(&self) -> Option<Expr> {
        child_after(&self.0, &[SHORT_EQUALS])
    }
}

impl IdentPattern {
    pub fn name(&self) -> String {
        token(&self.0, IDENTIFIER)
            .map(|t| t.text().to_string())
            .unwrap_or_default()
    }
}

impl ObjectPattern {
    pub fn entries(&self) -> Vec<ObjectPatternEntry> {
        children(&self.0)
    }
}

impl FullObjectPatternEntry {
    pub fn field(&self) -> String {
        token(&self.0, IDENTIFIER)
            .map(|t| t.text().to_string())
            .unwrap_or_default()
    }
    pub fn pattern(&self) -> Option<Pattern> {
        child(&self.0)
    }
}

impl ShorthandObjectPatternEntry {
    pub fn field(&self) -> String {
        token(&self.0, IDENTIFIER)
            .map(|t| t.text().to_string())
            .unwrap_or_default()
    }
}

impl ListPattern {
    pub fn items(&self) -> Vec<Pattern> {
        children(&self.0)
    }
}

// ------------------------------------------------------------------------------------------------ declarations

impl FunctionDecl {
    pub fn attributes(&self) -> Option<Attributes> {
        child(&self.0)
    }
    pub fn name_token(&self) -> Option<SyntaxToken> {
        token(&self.0, IDENTIFIER)
    }
    pub fn params(&self) -> Option<ParamList> {
        child(&self.0)
    }
    pub fn body(&self) -> Option<Expr> {
        child_after(&self.0, &[SHORT_EQUALS])
    }
    /// The doc comment: the `#` comment lines immediately preceding the declaration, if any
    pub fn doc_comment(&self) -> Option<DocComment> {
        doc_comment(&self.0)
    }
}

impl Attributes {
    pub fn attributes(&self) -> Vec<Attribute> {
        children(&self.0)
    }
}

impl Attribute {
    pub fn name(&self) -> String {
        token(&self.0, IDENTIFIER)
            .map(|t| t.text().to_string())
            .unwrap_or_default()
    }
}

impl ClassDecl {
    pub fn attributes(&self) -> Option<Attributes> {
        child(&self.0)
    }
    pub fn name_token(&self) -> Option<SyntaxToken> {
        token(&self.0, IDENTIFIER)
    }
    pub fn params(&self) -> Option<ParamList> {
        child(&self.0)
    }
    pub fn methods(&self) -> Vec<FunctionDecl> {
        child::<ClassBody>(&self.0).map(|b| children(&b.0)).unwrap_or_default()
    }
    pub fn doc_comment(&self) -> Option<DocComment> {
        doc_comment(&self.0)
    }
}

/// The raw text of the doc comment before a declaration: the contiguous `#` comment lines immediately
/// preceding it, with the `#`s removed. A blank line ends the comment block.
///
/// Comments are trivia, and may belong to an ancestor rather than to the declaration's parent (e.g. before
/// the first statement of a sequence), so this walks back through the token stream rather than siblings.
fn doc_comment_text(node: &SyntaxNode) -> Option<String> {
    let comments = doc_comment_tokens(node);
    if comments.is_empty() {
        return None;
    }
    let lines: Vec<&str> = comments
        .iter()
        .map(|t| {
            t.text()
                .strip_prefix('#')
                .unwrap_or(t.text())
                .trim_end_matches(['\r', '\n'])
        })
        .collect();
    Some(lines.join("\n"))
}

/// The comment tokens making up a declaration's doc comment, in source order.
pub(crate) fn doc_comment_tokens(node: &SyntaxNode) -> Vec<SyntaxToken> {
    let mut comments = vec![];
    let mut token = node.first_token().and_then(|t| t.prev_token());
    while let Some(t) = token {
        match t.kind() {
            COMMENT => comments.push(t.clone()),
            // A comment token includes its line ending, so any newline in the whitespace means a blank line
            WHITESPACE if !t.text().contains('\n') => {}
            _ => break,
        }
        token = t.prev_token();
    }
    comments.reverse();
    comments
}

fn doc_comment(node: &SyntaxNode) -> Option<DocComment> {
    doc_comment_text(node).map(|text| crate::doc_comment::parse(&text))
}

// ------------------------------------------------------------------------------------------------ mish

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MishItem {
    Word(SyntaxToken),
    Literal(Literal),
    String(InterpolatedString),
    Simple(SimpleInterpolation),
    Complex(ComplexInterpolation),
    Redirect(MishRedirect),
}

fn mish_items(node: &SyntaxNode) -> Vec<MishItem> {
    let mut items = vec![];
    for element in node.children_with_tokens() {
        match element {
            NodeOrToken::Token(t) if t.kind() == MISH_WORD => items.push(MishItem::Word(t)),
            NodeOrToken::Token(_) => {}
            NodeOrToken::Node(n) => match n.kind() {
                LITERAL => items.push(MishItem::Literal(Literal(n))),
                INTERPOLATED_STRING => items.push(MishItem::String(InterpolatedString(n))),
                SIMPLE_INTERPOLATION => items.push(MishItem::Simple(SimpleInterpolation(n))),
                COMPLEX_INTERPOLATION => items.push(MishItem::Complex(ComplexInterpolation(n))),
                MISH_REDIRECT => items.push(MishItem::Redirect(MishRedirect(n))),
                _ => {}
            },
        }
    }
    items
}

impl MishInterpolationExpr {
    /// `!{...}` captures output; `!!{...}` doesn't
    pub fn captures(&self) -> bool {
        token(&self.0, MISH_INTERPOLATION_START).is_some()
    }
    pub fn mish(&self) -> Option<MishExpr> {
        child(&self.0)
    }
}

impl MishExpr {
    pub fn items(&self) -> Vec<MishItem> {
        mish_items(&self.0)
    }
}

impl MishRedirect {
    pub fn op(&self) -> Option<SyntaxKind> {
        tokens(&self.0)
            .map(|t| t.kind())
            .find(|k| matches!(k, LESS_THAN | GREATER_THAN))
    }
    pub fn item(&self) -> Option<MishItem> {
        mish_items(&self.0).into_iter().next()
    }
}
