//! Hand-written recursive-descent parser, a port of `MashParse` and friends, building a rowan green tree directly.
//!
//! Differences from the Scala parser:
//! * No synthetic tokens. A missing token is just an error; the node lacks that child.
//! * Unexpected tokens are wrapped in `ERROR` nodes so the tree is always lossless.
//! * No `speculate()`/backtracking. Every place Scala speculated is decided up front by a cheap
//!   bracket-balanced lookahead scan over the token stream (see `scan_*`). This lets us build the
//!   rowan tree in a single pass with `GreenNodeBuilder` (which can't un-build).
//! * Inferred semicolons are zero-width "virtual" tokens: they affect parsing but don't appear in the
//!   tree (the newline trivia is still there, so the tree stays lossless).

use crate::lexer::Token;
use crate::{MAX_NESTING_DEPTH, SyntaxError, SyntaxKind, SyntaxKind::*};
use rowan::{Checkpoint, GreenNode, GreenNodeBuilder, TextRange, TextSize};

pub(crate) struct Parser<'t> {
    text: &'t str,
    /// All tokens including trivia
    raw: &'t [Token],
    /// Indices into `raw` of the non-trivia tokens
    significant: Vec<usize>,
    /// For each significant token: is it a candidate for an inferred semicolon before it?
    semi_candidate: Vec<bool>,
    /// For each significant token that opens a bracketed group: the offset just after its matching closer
    /// (brackets are matched by depth, not type). Lets lookahead scans skip a group in O(1).
    group_end: Vec<Option<usize>>,
    /// Current position in `significant`
    pos: usize,
    /// Next raw token not yet added to the tree
    raw_pos: usize,
    /// `pos` at which we've already handed out a virtual semicolon
    inferred_semi_consumed_at: Option<usize>,
    semis_allowed: bool,
    /// Current recursion depth in grammar levels (see [`MAX_NESTING_DEPTH`])
    depth: u32,
    /// Set once the nesting limit is hit: the rest of the input has been consumed as an error, and further
    /// errors (which would all be consequences) are suppressed.
    too_deep: bool,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<SyntaxError>,
}

/// Tokens that can start a statement, for semicolon inference.
/// DOT/DOT_NULL_SAFE deliberately excluded so a line starting with `.foo` continues the previous one.
fn can_start_statement(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        IDENTIFIER
            | RIGHT_ARROW
            | MINUS
            | LPAREN
            | LBRACE
            | LSQUARE
            | HOLE
            | TRUE_KW
            | FALSE_KW
            | STRING_LITERAL
            | NUMBER_LITERAL
            | NULL_KW
            | IF_KW
            | STRING_START
            | STRING_INTERPOLATION_START_COMPLEX
            | STRING_INTERPOLATION_START_SIMPLE
            | MISH_INTERPOLATION_START
            | MISH_INTERPOLATION_START_NO_CAPTURE
            | DEF_KW
            | CLASS_KW
            | THIS_KW
            | AT
            | IMPORT_KW
    )
}

fn can_end_statement(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        IDENTIFIER
            | RPAREN
            | RBRACE
            | RSQUARE
            | HOLE
            | TRUE_KW
            | FALSE_KW
            | STRING_LITERAL
            | NUMBER_LITERAL
            | NULL_KW
            | STRING_END
            | MISH_WORD
            | QUESTION
            | THIS_KW
            | LONG_FLAG
            | SHORT_FLAG
    )
}

fn cannot_start_arg(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        PIPE | RPAREN
            | EOF
            | LONG_EQUALS
            | NOT_EQUALS
            | GREATER_THAN
            | GREATER_THAN_EQUALS
            | LESS_THAN
            | LESS_THAN_EQUALS
            | AND_KW
            | OR_KW
            | PLUS
            | MINUS
            | TIMES
            | DIVIDE
            | IF_KW
            | THEN_KW
            | ELSE_KW
            | SEMI
            | COMMA
            | RSQUARE
            | ERROR_TOKEN
            | RBRACE
            | COLON
            | RIGHT_ARROW
            | SHORT_EQUALS
            | PLUS_EQUALS
            | MINUS_EQUALS
            | TIMES_EQUALS
            | DIVIDE_EQUALS
            | TILDE
            | DEF_KW
            | AT
            | CLASS_KW
            | STRING_END
            | ELLIPSIS
            | NAMESPACE_KW
            | IMPORT_KW
    )
}

/// Pseudo-kind for end of input (never appears in the tree)
const EOF: SyntaxKind = SyntaxKind::__LAST;

fn is_param_start(kind: SyntaxKind) -> bool {
    matches!(kind, IDENTIFIER | LPAREN | LBRACE | LSQUARE | HOLE)
}

impl<'t> Parser<'t> {
    pub(crate) fn new(text: &'t str, raw: &'t [Token]) -> Self {
        let significant: Vec<usize> = (0..raw.len()).filter(|&i| !raw[i].kind.is_trivia()).collect();
        let mut semi_candidate = vec![false; significant.len()];
        for i in 1..significant.len() {
            let (prev, cur) = (significant[i - 1], significant[i]);
            let newline_between = raw[prev + 1..cur].iter().any(|t| text[t.range].contains('\n'));
            semi_candidate[i] =
                newline_between && can_end_statement(raw[prev].kind) && can_start_statement(raw[cur].kind);
        }
        let mut group_end = vec![None; significant.len()];
        let mut open_groups = vec![];
        for (i, &raw_index) in significant.iter().enumerate() {
            let kind = raw[raw_index].kind;
            if kind.is_opener() {
                open_groups.push(i);
            } else if kind.is_closer()
                && let Some(opener) = open_groups.pop()
            {
                group_end[opener] = Some(i + 1);
            }
        }
        Parser {
            text,
            raw,
            significant,
            semi_candidate,
            group_end,
            pos: 0,
            raw_pos: 0,
            inferred_semi_consumed_at: None,
            semis_allowed: true,
            depth: 0,
            too_deep: false,
            builder: GreenNodeBuilder::new(),
            errors: vec![],
        }
    }

    pub(crate) fn finish(mut self) -> (GreenNode, Vec<SyntaxError>) {
        self.flush_trivia_until(self.raw.len());
        (self.builder.finish(), self.errors)
    }

    // ---------------------------------------------------------------- token access

    fn nth_real(&self, n: usize) -> SyntaxKind {
        self.significant
            .get(self.pos + n)
            .map(|&i| self.raw[i].kind)
            .unwrap_or(EOF)
    }

    fn virtual_semi_here(&self) -> bool {
        self.semis_allowed
            && self.semi_candidate.get(self.pos).copied().unwrap_or(false)
            && self.inferred_semi_consumed_at != Some(self.pos)
    }

    /// Current token kind, taking inferred semicolons into account
    fn current(&self) -> SyntaxKind {
        if self.virtual_semi_here() {
            SEMI
        } else {
            self.nth_real(0)
        }
    }

    fn at(&self, kind: SyntaxKind) -> bool {
        self.current() == kind
    }

    fn at_any(&self, kinds: &[SyntaxKind]) -> bool {
        kinds.contains(&self.current())
    }

    fn at_eof(&self) -> bool {
        self.current() == EOF
    }

    /// Lookahead ignoring semicolon inference (n = 0 is the current real token)
    fn nth(&self, n: usize) -> SyntaxKind {
        self.nth_real(n)
    }

    fn current_range(&self) -> TextRange {
        match self.significant.get(self.pos) {
            Some(&i) => self.raw[i].range,
            None => TextRange::empty(TextSize::of(self.text)),
        }
    }

    // ---------------------------------------------------------------- tree building

    fn flush_trivia_until(&mut self, raw_end: usize) {
        while self.raw_pos < raw_end {
            let t = self.raw[self.raw_pos];
            self.builder.token(t.kind.into(), &self.text[t.range]);
            self.raw_pos += 1;
        }
    }

    fn flush_leading_trivia(&mut self) {
        let end = self.significant.get(self.pos).copied().unwrap_or(self.raw.len());
        // At EOF, leave trailing trivia for the root
        if self.pos < self.significant.len() {
            self.flush_trivia_until(end);
        }
    }

    fn start(&mut self, kind: SyntaxKind) {
        self.flush_leading_trivia();
        self.builder.start_node(kind.into());
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    fn checkpoint(&mut self) -> Checkpoint {
        self.flush_leading_trivia();
        self.builder.checkpoint()
    }

    fn wrap(&mut self, cp: Checkpoint, kind: SyntaxKind) {
        self.builder.start_node_at(cp, kind.into());
        self.builder.finish_node();
    }

    /// Consume the current (real) token into the tree
    fn bump(&mut self) {
        if self.virtual_semi_here() {
            self.inferred_semi_consumed_at = Some(self.pos);
            return;
        }
        let Some(&i) = self.significant.get(self.pos) else {
            return;
        };
        self.flush_trivia_until(i);
        let t = self.raw[i];
        self.builder.token(t.kind.into(), &self.text[t.range]);
        self.raw_pos = i + 1;
        self.pos += 1;
        self.inferred_semi_consumed_at = None;
    }

    fn error(&mut self, message: impl Into<String>) {
        if self.too_deep {
            return;
        }
        let range = self.current_range();
        self.errors.push(SyntaxError {
            message: message.into(),
            range,
        });
    }

    fn expect(&mut self, kind: SyntaxKind, context: &str) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            self.error(format!("While parsing {context}, expected '{}'", kind.describe()));
            false
        }
    }

    fn eat(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn bump_as_error(&mut self, message: &str) {
        self.error(message.to_string());
        self.start(ERROR);
        self.bump();
        self.finish_node();
    }

    /// Run `f` one grammar level deeper. Every recursive cycle in the grammar passes through a call to this,
    /// which bounds native stack use.
    fn nested(&mut self, f: impl FnOnce(&mut Self)) {
        if self.deepen() {
            f(self);
            self.depth -= 1;
        }
    }

    /// Account for one more level of tree depth. Besides recursion (see [`Self::nested`]), loops that wrap
    /// the node built so far (`a + b + c`, `a.b.c`, `a | b | c`) make the tree deeper without recursing, and
    /// count too, so that later passes over the tree are bounded as well. Callers restore `depth` themselves.
    ///
    /// Past [`MAX_NESTING_DEPTH`], the rest of the input becomes a single `ERROR` node and `false` is returned.
    fn deepen(&mut self) -> bool {
        if self.too_deep {
            return false;
        }
        if self.depth >= MAX_NESTING_DEPTH {
            self.error("Expression is too deeply nested");
            self.too_deep = true;
            self.start(ERROR);
            while self.pos < self.significant.len() {
                self.bump();
            }
            self.finish_node();
            return false;
        }
        self.depth += 1;
        true
    }

    fn with_semis<T>(&mut self, allowed: bool, f: impl FnOnce(&mut Self) -> T) -> T {
        let old = self.semis_allowed;
        self.semis_allowed = allowed;
        let result = f(self);
        self.semis_allowed = old;
        result
    }

    // ---------------------------------------------------------------- lookahead scans (replace speculate())

    /// Skip one bracket-balanced group starting at significant offset `i` (which must be an opener).
    /// Returns the offset after the matching closer, or None if unbalanced before EOF.
    fn skip_group(&self, i: usize) -> Option<usize> {
        let end = self.group_end.get(self.pos + i).copied().flatten()?;
        Some(end - self.pos)
    }

    /// Is a lambda starting here? i.e. `param* =>` where params are identifiers (optionally `...`), holes,
    /// or bracketed groups. Replaces Scala's `speculate("lambdaStart")`.
    fn scan_lambda_start(&self) -> bool {
        let mut i = 0;
        loop {
            match self.nth(i) {
                RIGHT_ARROW => return true,
                IDENTIFIER => {
                    i += 1;
                    if self.nth(i) == ELLIPSIS {
                        i += 1;
                    }
                }
                HOLE => i += 1,
                LPAREN | LBRACE | LSQUARE => match self.skip_group(i) {
                    Some(next) => i = next,
                    None => return false,
                },
                _ => return false,
            }
        }
    }

    /// Is `pattern =` starting here? Replaces Scala's `speculate("patternEquals")`.
    fn scan_pattern_assignment(&self) -> bool {
        let after = match self.nth(0) {
            IDENTIFIER | HOLE => Some(1),
            LBRACE | LSQUARE => self.skip_group(0),
            _ => None,
        };
        after.is_some_and(|i| self.nth(i) == SHORT_EQUALS)
    }

    /// At `{`: is this an object literal (vs a block)? Replaces Scala's `speculate("objectExpr")`.
    ///
    /// Object if: `{}`; or a `:` at depth 0 before any statement separator; or the first entry is a lone
    /// identifier followed by `,` or `}` (shorthand `{ a }`, `{ a, b }`).
    fn scan_object_literal(&self) -> bool {
        if self.nth(1) == RBRACE {
            return true;
        }
        if self.nth(1) == IDENTIFIER && matches!(self.nth(2), COMMA | RBRACE) {
            return true;
        }
        let mut i = 1;
        loop {
            let k = self.nth(i);
            match k {
                COLON => return true,
                EOF | SEMI | RBRACE | RPAREN | RSQUARE | COMMA | RIGHT_ARROW => return false,
                _ if self.semi_candidate.get(self.pos + i).copied().unwrap_or(false) => return false,
                _ if k.is_opener() => match self.skip_group(i) {
                    Some(next) => i = next,
                    None => return false,
                },
                _ => i += 1,
            }
        }
    }

    // ---------------------------------------------------------------- grammar: top level

    pub(crate) fn source_file(&mut self) {
        self.builder.start_node(SOURCE_FILE.into());
        if self.at(NAMESPACE_KW) {
            self.namespace_decl();
        }
        self.statement_seq();
        // Error recovery: skip the offending token and carry on parsing statements
        while !self.at_eof() {
            self.bump_as_error("Unexpected token");
            self.statement_seq_inner(false);
        }
        self.flush_trivia_until(self.raw.len());
        self.builder.finish_node();
    }

    pub(crate) fn mish_file(&mut self) {
        self.builder.start_node(SOURCE_FILE.into());
        self.mish_expr();
        while !self.at_eof() {
            self.bump_as_error("Unexpected token");
        }
        self.flush_trivia_until(self.raw.len());
        self.builder.finish_node();
    }

    fn namespace_decl(&mut self) {
        self.start(NAMESPACE_DECL);
        self.bump();
        self.expect(IDENTIFIER, "namespace");
        while self.at(DOT) {
            self.bump();
            self.expect(IDENTIFIER, "namespace");
        }
        self.finish_node();
    }

    fn statement_seq(&mut self) {
        self.statement_seq_inner(true)
    }

    /// A sequence of statements separated by (possibly inferred) semicolons. Produces a STATEMENT_SEQ node
    /// unless there's exactly one statement and no semicolons (as in Scala). `wrap_empty`: whether an empty
    /// sequence still gets a (zero-width) node, as `()` or an empty program need one.
    fn statement_seq_inner(&mut self, wrap_empty: bool) {
        if self.scan_lambda_start() {
            self.lambda_expr(LambdaBody::StatementSeq);
            return;
        }
        let cp = self.checkpoint();
        let mut statements = 0;
        let mut saw_semi = false;
        loop {
            if self.at_any(&[RBRACE, RPAREN, EOF]) {
                break;
            }
            if self.at(SEMI) {
                self.bump();
                saw_semi = true;
                continue;
            }
            let before = self.pos;
            self.statement_expr();
            if self.pos != before {
                statements += 1;
            }
            if self.at(SEMI) {
                self.bump();
                saw_semi = true;
            } else {
                // Either the statement ended without a separator, or it couldn't be parsed at all: stop
                break;
            }
        }
        let single = statements == 1 && !saw_semi;
        let empty = statements == 0 && !saw_semi;
        if !single && (wrap_empty || !empty) {
            self.wrap(cp, STATEMENT_SEQ);
        }
    }

    fn statement_expr(&mut self) {
        match self.current() {
            AT => {
                self.with_semis(false, |p| {
                    let cp = p.checkpoint();
                    p.attributes();
                    if p.at(CLASS_KW) {
                        p.class_decl(cp)
                    } else {
                        p.function_decl(cp)
                    }
                });
            }
            DEF_KW => {
                let cp = self.checkpoint();
                self.function_decl(cp)
            }
            CLASS_KW => {
                let cp = self.checkpoint();
                self.class_decl(cp)
            }
            IMPORT_KW => self.import_stmt(),
            _ => self.pipe_expr(),
        }
    }

    fn import_stmt(&mut self) {
        self.start(IMPORT_STMT);
        self.bump();
        self.expect(IDENTIFIER, "import statement");
        loop {
            if !self.expect(DOT, "import statement") {
                break;
            }
            if self.at(HOLE) || (self.at(IDENTIFIER) && self.nth(1) != DOT) {
                self.bump();
                break;
            } else if self.at(IDENTIFIER) {
                self.bump();
            } else {
                self.error("While parsing import statement, expected identifier");
                break;
            }
        }
        self.finish_node();
    }

    // ---------------------------------------------------------------- grammar: expressions

    fn pipe_expr(&mut self) {
        self.nested(|p| p.pipe_expr_inner())
    }

    fn pipe_expr_inner(&mut self) {
        let cp = self.checkpoint();
        self.lambda_expr(LambdaBody::Pipe);
        let depth = self.depth;
        while self.at(PIPE) && self.deepen() {
            self.bump();
            self.lambda_expr(LambdaBody::NoPipe);
            self.wrap(cp, PIPE_EXPR);
        }
        self.depth = depth;
    }

    fn lambda_expr(&mut self, body: LambdaBody) {
        if !self.scan_lambda_start() {
            return self.pattern_assignment_expr();
        }
        self.start(LAMBDA_EXPR);
        self.param_list();
        self.expect(RIGHT_ARROW, "lambda");
        match body {
            LambdaBody::StatementSeq => self.nested(|p| p.statement_seq()),
            LambdaBody::Pipe => self.pipe_expr(),
            LambdaBody::NoPipe => self.nested(|p| p.lambda_expr(LambdaBody::NoPipe)),
        }
        self.finish_node();
    }

    fn pattern_assignment_expr(&mut self) {
        if !self.scan_pattern_assignment() {
            return self.assignment_expr();
        }
        self.start(PATTERN_ASSIGNMENT_EXPR);
        self.pattern();
        self.expect(SHORT_EQUALS, "assignment");
        self.pipe_expr();
        self.finish_node();
    }

    fn assignment_expr(&mut self) {
        let cp = self.checkpoint();
        self.if_expr();
        if self.at_any(&[SHORT_EQUALS, PLUS_EQUALS, MINUS_EQUALS, TIMES_EQUALS, DIVIDE_EQUALS]) {
            self.bump();
            self.pipe_expr();
            self.wrap(cp, ASSIGNMENT_EXPR);
        }
    }

    fn if_expr(&mut self) {
        if !self.at(IF_KW) {
            return self.or_expr();
        }
        self.start(IF_EXPR);
        self.bump();
        self.or_expr();
        self.expect(THEN_KW, "if expression");
        self.pipe_expr();
        if self.eat(ELSE_KW) {
            self.pipe_expr();
        }
        self.finish_node();
    }

    fn or_expr(&mut self) {
        let cp = self.checkpoint();
        self.and_expr();
        if self.at(OR_KW) {
            self.bump();
            self.nested(|p| p.or_expr());
            self.wrap(cp, BIN_OP_EXPR);
        }
    }

    fn and_expr(&mut self) {
        let cp = self.checkpoint();
        self.comparison_expr();
        if self.at(AND_KW) {
            self.bump();
            self.nested(|p| p.and_expr());
            self.wrap(cp, BIN_OP_EXPR);
        }
    }

    fn comparison_expr(&mut self) {
        let cp = self.checkpoint();
        self.additive_expr();
        let op = self.current();
        if !matches!(
            op,
            LONG_EQUALS | NOT_EQUALS | GREATER_THAN | GREATER_THAN_EQUALS | LESS_THAN | LESS_THAN_EQUALS
        ) {
            return;
        }
        self.bump();
        self.additive_expr();
        let chain_kinds: &[SyntaxKind] = match op {
            LESS_THAN | LESS_THAN_EQUALS => &[LESS_THAN, LESS_THAN_EQUALS],
            GREATER_THAN | GREATER_THAN_EQUALS => &[GREATER_THAN, GREATER_THAN_EQUALS],
            _ => &[],
        };
        if self.at_any(chain_kinds) {
            while self.at_any(chain_kinds) {
                self.bump();
                self.additive_expr();
            }
            self.wrap(cp, CHAINED_OP_EXPR);
        } else {
            self.wrap(cp, BIN_OP_EXPR);
        }
    }

    fn additive_expr(&mut self) {
        let cp = self.checkpoint();
        self.multiplicative_expr();
        let depth = self.depth;
        while self.at_any(&[PLUS, MINUS]) && self.deepen() {
            self.bump();
            self.multiplicative_expr();
            self.wrap(cp, BIN_OP_EXPR);
        }
        self.depth = depth;
    }

    fn multiplicative_expr(&mut self) {
        let cp = self.checkpoint();
        self.invocation_expr();
        let depth = self.depth;
        while self.at_any(&[TIMES, DIVIDE]) && self.deepen() {
            self.bump();
            self.invocation_expr();
            self.wrap(cp, BIN_OP_EXPR);
        }
        self.depth = depth;
    }

    fn invocation_expr(&mut self) {
        let cp = self.checkpoint();
        self.prefix_expr();
        let mut any_args = false;
        while !cannot_start_arg(self.current()) {
            let before = self.pos;
            self.arg();
            if self.pos == before {
                break; // no progress (error already recorded)
            }
            any_args = true;
        }
        if any_args {
            self.wrap(cp, INVOCATION_EXPR);
        }
    }

    fn arg(&mut self) {
        match self.current() {
            SHORT_FLAG => {
                self.start(SHORT_ARG);
                self.bump();
                self.finish_node();
            }
            LONG_FLAG => {
                self.start(LONG_ARG);
                self.bump();
                if self.eat(SHORT_EQUALS) {
                    self.prefix_expr();
                }
                self.finish_node();
            }
            _ => self.prefix_expr(),
        }
    }

    fn prefix_expr(&mut self) {
        match self.current() {
            MINUS => {
                self.start(MINUS_EXPR);
                self.bump();
                self.nested(|p| p.prefix_expr());
                self.finish_node();
            }
            DOT | DOT_NULL_SAFE => {
                let cp = self.checkpoint();
                self.start(HEADLESS_MEMBER_EXPR);
                self.bump();
                self.expect(IDENTIFIER, "member expression");
                self.finish_node();
                self.continue_suffix_expr(cp);
            }
            _ => self.suffix_expr(),
        }
    }

    fn suffix_expr(&mut self) {
        let cp = self.checkpoint();
        self.primary_expr();
        self.continue_suffix_expr(cp);
    }

    fn continue_suffix_expr(&mut self, cp: Checkpoint) {
        let depth = self.depth;
        loop {
            let kind = self.current();
            if !matches!(kind, DOT | DOT_NULL_SAFE | LSQUARE_LOOKUP | LPAREN_INVOKE | QUESTION) || !self.deepen() {
                break;
            }
            match kind {
                DOT | DOT_NULL_SAFE => {
                    self.bump();
                    self.expect(IDENTIFIER, "member expression");
                    self.wrap(cp, MEMBER_EXPR);
                }
                LSQUARE_LOOKUP => {
                    self.bump();
                    self.pipe_expr();
                    self.expect(RSQUARE, "lookup expression");
                    self.wrap(cp, LOOKUP_EXPR);
                }
                LPAREN_INVOKE => {
                    self.bump();
                    if !self.eat(RPAREN) {
                        self.statement_expr();
                        while self.eat(COMMA) {
                            self.statement_expr();
                        }
                        self.expect(RPAREN, "invocation");
                    }
                    self.wrap(cp, PAREN_INVOCATION_EXPR);
                }
                _ => {
                    // QUESTION
                    self.bump();
                    self.wrap(cp, HELP_EXPR);
                }
            }
        }
        self.depth = depth;
    }

    fn primary_expr(&mut self) {
        match self.current() {
            NUMBER_LITERAL | STRING_LITERAL | TRUE_KW | FALSE_KW | NULL_KW => self.single_token_node(LITERAL),
            THIS_KW => self.single_token_node(THIS_EXPR),
            STRING_START => self.interpolated_string(),
            IDENTIFIER => self.single_token_node(IDENTIFIER_EXPR),
            MISH_WORD => self.single_token_node(MISH_FUNCTION),
            HOLE => self.single_token_node(HOLE_EXPR),
            LPAREN => {
                self.start(PAREN_EXPR);
                self.bump();
                self.statement_seq();
                self.expect(RPAREN, "parenthesis expression");
                self.finish_node();
            }
            LSQUARE => self.list_expr(),
            LBRACE => {
                if self.scan_object_literal() {
                    self.object_expr()
                } else {
                    self.block_expr()
                }
            }
            MISH_INTERPOLATION_START | MISH_INTERPOLATION_START_NO_CAPTURE => {
                self.start(MISH_INTERPOLATION_EXPR);
                self.bump();
                self.mish_expr();
                self.expect(RBRACE, "mish interpolation");
                self.finish_node();
            }
            EOF => self.error("Unexpected end of input, expected expression"),
            _ => self.error("Expected expression"),
        }
    }

    fn single_token_node(&mut self, kind: SyntaxKind) {
        self.start(kind);
        self.bump();
        self.finish_node();
    }

    fn list_expr(&mut self) {
        self.start(LIST_EXPR);
        self.bump();
        if !self.eat(RSQUARE) {
            self.statement_expr();
            while self.eat(COMMA) {
                self.statement_expr();
            }
            self.expect(RSQUARE, "list");
        }
        self.finish_node();
    }

    fn object_expr(&mut self) {
        self.start(OBJECT_EXPR);
        self.bump();
        if !self.eat(RBRACE) {
            self.object_entry();
            while self.eat(COMMA) {
                self.object_entry();
            }
            self.expect(RBRACE, "object");
        }
        self.finish_node();
    }

    fn object_entry(&mut self) {
        if self.at(IDENTIFIER) && matches!(self.nth(1), RBRACE | COMMA) {
            self.single_token_node(SHORTHAND_OBJECT_ENTRY);
        } else {
            self.start(FULL_OBJECT_ENTRY);
            self.suffix_expr();
            self.expect(COLON, "object");
            self.statement_expr();
            self.finish_node();
        }
    }

    fn block_expr(&mut self) {
        self.start(BLOCK_EXPR);
        self.bump();
        self.with_semis(true, |p| p.statement_seq());
        self.expect(RBRACE, "block");
        self.finish_node();
    }

    // ---------------------------------------------------------------- strings

    fn interpolated_string(&mut self) {
        self.start(INTERPOLATED_STRING);
        self.bump(); // STRING_START
        while self.at_any(&[
            STRING_MIDDLE,
            STRING_INTERPOLATION_START_COMPLEX,
            STRING_INTERPOLATION_START_SIMPLE,
        ]) {
            self.interpolation_part();
        }
        self.expect(STRING_END, "interpolated string");
        self.finish_node();
    }

    fn interpolation_part(&mut self) {
        match self.current() {
            STRING_MIDDLE => self.bump(),
            STRING_INTERPOLATION_START_COMPLEX => {
                self.start(COMPLEX_INTERPOLATION);
                self.bump();
                self.pipe_expr();
                self.expect(RBRACE, "interpolation");
                self.finish_node();
            }
            _ => {
                self.start(SIMPLE_INTERPOLATION);
                self.bump(); // $
                let cp = self.checkpoint();
                match self.current() {
                    HOLE => self.single_token_node(HOLE_EXPR),
                    IDENTIFIER => self.single_token_node(IDENTIFIER_EXPR),
                    _ => self.error("While parsing interpolation, expected identifier or _"),
                }
                while self.at(DOT) {
                    self.bump();
                    self.expect(IDENTIFIER, "interpolation");
                    self.wrap(cp, MEMBER_EXPR);
                }
                self.finish_node();
            }
        }
    }

    // ---------------------------------------------------------------- mish

    fn is_mish_item_start(&self) -> bool {
        self.at_any(&[
            MISH_WORD,
            STRING_LITERAL,
            STRING_START,
            STRING_INTERPOLATION_START_SIMPLE,
            STRING_INTERPOLATION_START_COMPLEX,
            LESS_THAN,
            GREATER_THAN,
        ])
    }

    fn mish_expr(&mut self) {
        self.start(MISH_EXPR);
        self.mish_item();
        while self.is_mish_item_start() {
            self.mish_item();
        }
        self.finish_node();
    }

    fn mish_item(&mut self) {
        match self.current() {
            MISH_WORD => self.bump(),
            STRING_LITERAL => self.single_token_node(LITERAL),
            STRING_START => self.interpolated_string(),
            STRING_INTERPOLATION_START_SIMPLE | STRING_INTERPOLATION_START_COMPLEX => self.interpolation_part(),
            LESS_THAN | GREATER_THAN => {
                self.start(MISH_REDIRECT);
                self.bump();
                self.nested(|p| p.mish_item());
                self.finish_node();
            }
            _ => self.error("Expected command"),
        }
    }

    // ---------------------------------------------------------------- declarations

    fn attributes(&mut self) {
        self.start(ATTRIBUTES);
        while self.at(AT) {
            self.start(ATTRIBUTE);
            self.bump();
            if self.eat(LPAREN) {
                self.expect(IDENTIFIER, "attribute");
                while !cannot_start_arg(self.current()) {
                    let before = self.pos;
                    self.arg();
                    if self.pos == before {
                        break;
                    }
                }
                self.expect(RPAREN, "attribute");
            } else {
                self.expect(IDENTIFIER, "attribute");
            }
            self.finish_node();
        }
        self.finish_node();
    }

    /// `cp` is before any attributes
    fn function_decl(&mut self, cp: Checkpoint) {
        self.with_semis(false, |p| {
            p.builder.start_node_at(cp, FUNCTION_DECL.into());
            p.expect(DEF_KW, "function");
            p.expect(IDENTIFIER, "function");
            p.param_list();
            p.expect(SHORT_EQUALS, "function");
            p.with_semis(true, |p| p.pipe_expr());
            p.finish_node();
        })
    }

    fn class_decl(&mut self, cp: Checkpoint) {
        self.with_semis(false, |p| {
            p.builder.start_node_at(cp, CLASS_DECL.into());
            p.expect(CLASS_KW, "class");
            p.expect(IDENTIFIER, "class");
            // As in Scala, a newline after the name ends the declaration
            p.with_semis(true, |p| p.class_params_and_body());
            p.finish_node();
        })
    }

    fn class_params_and_body(&mut self) {
        let p = self;
        {
            if p.at_any(&[IDENTIFIER, LPAREN, LSQUARE, HOLE]) {
                p.start(PARAM_LIST);
                while p.at_any(&[IDENTIFIER, LPAREN, LSQUARE, HOLE]) {
                    p.parameter();
                }
                p.finish_node();
            }
            if p.at(LBRACE) {
                p.start(CLASS_BODY);
                p.bump();
                p.with_semis(true, |p| {
                    while p.at_any(&[DEF_KW, AT, SEMI]) {
                        if p.eat(SEMI) {
                            continue;
                        }
                        let cp = p.checkpoint();
                        if p.at(AT) {
                            p.attributes();
                        }
                        p.function_decl(cp);
                    }
                });
                p.expect(RBRACE, "class");
                p.finish_node();
            }
        }
    }

    fn param_list(&mut self) {
        if !is_param_start(self.current()) {
            return;
        }
        self.start(PARAM_LIST);
        self.parameter();
        self.with_semis(false, |p| {
            while is_param_start(p.current()) {
                p.parameter();
            }
        });
        self.finish_node();
    }

    fn parameter(&mut self) {
        match self.current() {
            IDENTIFIER => {
                self.start(SIMPLE_PARAM);
                self.single_token_node(IDENT_PATTERN);
                self.eat(ELLIPSIS);
                self.finish_node();
            }
            LPAREN => {
                self.start(PAREN_PARAM);
                self.bump();
                if self.at(AT) {
                    self.attributes();
                }
                self.start(SIMPLE_PARAM);
                self.pattern();
                self.eat(ELLIPSIS);
                self.finish_node();
                if self.eat(SHORT_EQUALS) {
                    self.pipe_expr();
                }
                self.expect(RPAREN, "parameter");
                self.finish_node();
            }
            LBRACE | LSQUARE | HOLE => {
                self.start(SIMPLE_PARAM);
                self.pattern();
                self.finish_node();
            }
            _ => self.error("While parsing parameter, expected identifier"),
        }
    }

    fn pattern(&mut self) {
        match self.current() {
            HOLE => self.single_token_node(HOLE_PATTERN),
            IDENTIFIER => self.single_token_node(IDENT_PATTERN),
            LSQUARE => {
                self.start(LIST_PATTERN);
                self.bump();
                if !self.eat(RSQUARE) {
                    self.nested(|p| p.pattern());
                    while self.eat(COMMA) {
                        self.nested(|p| p.pattern());
                    }
                    self.expect(RSQUARE, "list pattern");
                }
                self.finish_node();
            }
            LBRACE => {
                self.start(OBJECT_PATTERN);
                self.bump();
                if !self.eat(RBRACE) {
                    self.object_pattern_entry();
                    while self.eat(COMMA) {
                        self.object_pattern_entry();
                    }
                    self.expect(RBRACE, "object pattern");
                }
                self.finish_node();
            }
            _ => self.error("Expected pattern"),
        }
    }

    fn object_pattern_entry(&mut self) {
        if !self.at(IDENTIFIER) {
            return self.error("While parsing object pattern, expected identifier");
        }
        if self.nth(1) == COLON {
            self.start(FULL_OBJECT_PATTERN_ENTRY);
            self.bump();
            self.bump();
            self.nested(|p| p.pattern());
            self.finish_node();
        } else {
            self.single_token_node(SHORTHAND_OBJECT_PATTERN_ENTRY);
        }
    }
}

#[derive(Clone, Copy)]
enum LambdaBody {
    StatementSeq,
    Pipe,
    NoPipe,
}
