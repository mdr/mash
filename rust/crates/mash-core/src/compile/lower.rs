//! Lowering from the typed syntax tree to the evaluator's AST (Scala `Abstractifier`).
//!
//! Lowering never fails: parts missing from an incomplete parse become [`ExprKind::Missing`], so that the
//! IDE features can work on what's there.

use std::collections::HashMap;
use std::rc::Rc;

use mash_syntax::ast::{self, AstNode, LiteralValue, MishItem};
use mash_syntax::{SyntaxKind, TextRange};

use crate::ast::*;

/// Maps source ranges to the nodes lowered from them (innermost wins when ranges coincide), so IDE features
/// can get from a syntax node to its [`NodeId`] in an inference side table.
#[derive(Debug, Default)]
pub struct SourceMap {
    pub by_range: HashMap<TextRange, NodeId>,
}

pub(crate) struct Lowerer<'a> {
    ids: &'a mut NodeIdGen,
    unit: Rc<SourceUnit>,
    namespace: Option<Rc<[Rc<str>]>>,
    pub(crate) source_map: SourceMap,
}

fn bin_op(kind: SyntaxKind) -> Option<BinOp> {
    use SyntaxKind::*;
    Some(match kind {
        PLUS | PLUS_EQUALS => BinOp::Plus,
        MINUS | MINUS_EQUALS => BinOp::Minus,
        TIMES | TIMES_EQUALS => BinOp::Multiply,
        DIVIDE | DIVIDE_EQUALS => BinOp::Divide,
        LONG_EQUALS => BinOp::Equals,
        NOT_EQUALS => BinOp::NotEquals,
        LESS_THAN => BinOp::LessThan,
        LESS_THAN_EQUALS => BinOp::LessThanEquals,
        GREATER_THAN => BinOp::GreaterThan,
        GREATER_THAN_EQUALS => BinOp::GreaterThanEquals,
        AND_KW => BinOp::And,
        OR_KW => BinOp::Or,
        _ => return None,
    })
}

impl<'a> Lowerer<'a> {
    pub(crate) fn new(ids: &'a mut NodeIdGen, unit: Rc<SourceUnit>) -> Self {
        Lowerer {
            ids,
            unit,
            namespace: None,
            source_map: SourceMap::default(),
        }
    }

    pub(crate) fn source_file(&mut self, file: &ast::SourceFile) -> Program {
        self.namespace = file
            .namespace()
            .map(|ns| ns.segments().into_iter().map(Rc::from).collect());
        let body = if let Some(mish) = file.mish() {
            self.mish(&mish, false)
        } else {
            let mut exprs: Vec<E> = file.exprs().iter().map(|e| self.expr(e)).collect();
            match exprs.len() {
                1 => exprs.pop().unwrap(),
                _ => self.node(Some(file.syntax().text_range()), ExprKind::StatementSeq(exprs)),
            }
        };
        Program {
            namespace: self.namespace.clone(),
            body,
            unit: self.unit.clone(),
        }
    }

    fn node(&mut self, range: Option<TextRange>, kind: ExprKind) -> E {
        Expr::new(self.ids, range, kind)
    }

    fn missing(&mut self) -> E {
        self.node(None, ExprKind::Missing)
    }

    fn opt(&mut self, e: Option<ast::Expr>) -> E {
        match e {
            Some(e) => self.expr(&e),
            None => self.missing(),
        }
    }

    pub(crate) fn expr(&mut self, e: &ast::Expr) -> E {
        let range = e.syntax().text_range();
        let kind = self.kind(e);
        let node = self.node(Some(range), kind);
        self.source_map.by_range.entry(range).or_insert(node.id);
        node
    }

    fn kind(&mut self, e: &ast::Expr) -> ExprKind {
        use ast::Expr as A;
        match e {
            A::StatementSeq(s) => ExprKind::StatementSeq(s.statements().iter().map(|e| self.expr(e)).collect()),
            A::Literal(l) => self.literal(l),
            A::InterpolatedString(s) => ExprKind::Interpolated {
                parts: s
                    .parts()
                    .into_iter()
                    .map(|p| match p {
                        ast::InterpolationPart::Text(t) => InterpolationPart::Text(t.into()),
                        ast::InterpolationPart::Simple(s) => InterpolationPart::Expr(self.opt(s.expr())),
                        ast::InterpolationPart::Complex(c) => InterpolationPart::Expr(self.opt(c.expr())),
                    })
                    .collect(),
                tilde: s.tilde(),
            },
            A::Identifier(i) => ExprKind::Identifier(i.name().into()),
            A::Hole(h) => ExprKind::Hole(h.index().unwrap_or(1)),
            A::This(_) => ExprKind::This,
            A::MishFunction(m) => ExprKind::MishFunction(m.command().into()),
            A::Paren(p) => ExprKind::Paren(self.opt(p.expr())),
            A::List(l) => ExprKind::List(l.items().iter().map(|e| self.expr(e)).collect()),
            A::Object(o) => ExprKind::Object(
                o.entries()
                    .iter()
                    .map(|entry| match entry {
                        ast::ObjectEntry::Full(f) => ObjectEntry::Full {
                            field: self.opt(f.field()),
                            value: self.opt(f.value()),
                        },
                        ast::ObjectEntry::Shorthand(s) => ObjectEntry::Shorthand {
                            name: s.name().into(),
                            range: Some(s.syntax().text_range()),
                        },
                    })
                    .collect(),
            ),
            A::Block(b) => ExprKind::Block(self.opt(b.body())),
            A::Member(m) => ExprKind::Member {
                target: self.opt(m.target()),
                name: m.name_token().map(|t| t.text().into()).unwrap_or_else(|| "".into()),
                null_safe: m.is_null_safe(),
                name_range: m.name_token().map(|t| t.text_range()),
            },
            A::HeadlessMember(m) => ExprKind::HeadlessMember {
                name: m.name_token().map(|t| t.text().into()).unwrap_or_else(|| "".into()),
                null_safe: m.is_null_safe(),
            },
            A::Lookup(l) => ExprKind::Lookup {
                target: self.opt(l.target()),
                index: self.opt(l.index()),
            },
            A::ParenInvocation(p) => ExprKind::Invocation {
                function: self.opt(p.function()),
                args: p.args().iter().map(|a| Arg::Positional(self.expr(a))).collect(),
                paren: true,
            },
            A::Invocation(i) => {
                let function = self.opt(i.function());
                ExprKind::Invocation {
                    function,
                    args: self.args(i.args()),
                    paren: false,
                }
            }
            A::Help(h) => ExprKind::Help(self.opt(h.expr())),
            A::Minus(m) => ExprKind::Minus(self.opt(m.operand())),
            A::BinOp(b) => {
                let (lhs, rhs) = (self.opt(b.lhs()), self.opt(b.rhs()));
                match b.op().and_then(|t| bin_op(t.kind())) {
                    Some(op) => ExprKind::BinOp { op, lhs, rhs },
                    None => ExprKind::Missing,
                }
            }
            A::ChainedOp(c) => ExprKind::ChainedOp {
                first: self.opt(c.first()),
                rest: c
                    .rest()
                    .into_iter()
                    .filter_map(|(k, e)| Some((bin_op(k)?, self.opt(e))))
                    .collect(),
            },
            A::If(i) => ExprKind::If {
                cond: self.opt(i.condition()),
                then: self.opt(i.then_branch()),
                else_: i.else_branch().map(|e| self.expr(&e)),
            },
            A::Assignment(a) => ExprKind::Assignment {
                lhs: self.opt(a.lhs()),
                op: a.op().filter(|k| *k != SyntaxKind::SHORT_EQUALS).and_then(bin_op),
                rhs: self.opt(a.rhs()),
            },
            A::PatternAssignment(p) => {
                let rhs = self.opt(p.rhs());
                match p.pattern() {
                    // `a = 1` parses as a pattern assignment with an identifier pattern; it is an ordinary
                    // assignment
                    Some(ast::Pattern::Ident(i)) => {
                        let range = i.syntax().text_range();
                        let lhs = self.node(Some(range), ExprKind::Identifier(i.name().into()));
                        self.source_map.by_range.entry(range).or_insert(lhs.id);
                        ExprKind::Assignment { lhs, op: None, rhs }
                    }
                    Some(pattern) => ExprKind::PatternAssignment {
                        pattern: self.pattern(&pattern),
                        rhs,
                    },
                    None => ExprKind::Missing,
                }
            }
            A::Lambda(l) => {
                let params = l.params().map(|p| self.params(&p)).unwrap_or_default();
                let body = self.opt(l.body());
                ExprKind::Lambda(Rc::new(FnDef {
                    name: None,
                    doc: None,
                    attributes: vec![],
                    params,
                    body,
                    namespace: self.namespace.clone(),
                    unit: self.unit.clone(),
                    range: Some(l.syntax().text_range()),
                }))
            }
            A::Pipe(p) => ExprKind::Pipe {
                lhs: self.opt(p.lhs()),
                rhs: self.opt(p.rhs()),
            },
            A::FunctionDecl(f) => ExprKind::FunctionDecl(Rc::new(self.function_decl(f))),
            A::ClassDecl(c) => {
                let attributes = self.attributes(c.attributes());
                let params = c.params().map(|p| self.params(&p)).unwrap_or_default();
                let methods = c.methods().iter().map(|m| Rc::new(self.function_decl(m))).collect();
                ExprKind::ClassDecl(Rc::new(ClassDef {
                    name: c.name_token().map(|t| t.text().into()).unwrap_or_else(|| "".into()),
                    doc: c.doc_comment(),
                    attributes,
                    params,
                    methods,
                    namespace: self.namespace.clone(),
                    unit: self.unit.clone(),
                    range: Some(c.syntax().text_range()),
                }))
            }
            A::Import(i) => self.import(i),
            A::MishInterpolation(m) => match m.mish() {
                Some(mish) => self.mish_kind(&mish, m.captures()),
                None => ExprKind::Missing,
            },
        }
    }

    fn literal(&mut self, l: &ast::Literal) -> ExprKind {
        match l.value() {
            Some(LiteralValue::Number(n)) => ExprKind::Literal(Literal::Number(n)),
            Some(LiteralValue::String { double_quoted, .. }) => {
                // Only double-quoted strings can start with `~` (meaning the home directory)
                let raw = l.token().map(|t| t.text().to_string()).unwrap_or_default();
                let (value, tilde) = ast::string_text(&raw, double_quoted, true, true);
                let quote = if double_quoted {
                    QuoteKind::Double
                } else {
                    QuoteKind::Single
                };
                ExprKind::String {
                    value: value.into(),
                    quote,
                    tilde,
                }
            }
            Some(LiteralValue::Bool(b)) => ExprKind::Literal(Literal::Bool(b)),
            Some(LiteralValue::Null) => ExprKind::Literal(Literal::Null),
            None => ExprKind::Missing,
        }
    }

    fn args(&mut self, args: Vec<ast::Arg>) -> Vec<Arg> {
        args.into_iter()
            .map(|a| match a {
                ast::Arg::Positional(e) => Arg::Positional(self.expr(&e)),
                ast::Arg::Short(s) => Arg::ShortFlags {
                    flags: s.flags().chars().collect(),
                    range: Some(s.syntax().text_range()),
                },
                ast::Arg::Long(l) => Arg::LongFlag {
                    name: l.name().into(),
                    value: l.value().map(|v| self.expr(&v)),
                    range: Some(l.syntax().text_range()),
                },
            })
            .collect()
    }

    fn attributes(&mut self, attributes: Option<ast::Attributes>) -> Vec<Attribute> {
        let Some(attributes) = attributes else { return vec![] };
        attributes
            .attributes()
            .iter()
            .map(|a| Attribute {
                name: a.name().into(),
                // `@(name)` with no arguments is the same as `@name`
                args: a.args().map(|args| self.args(args)).filter(|args| !args.is_empty()),
                range: Some(a.syntax().text_range()),
            })
            .collect()
    }

    fn function_decl(&mut self, f: &ast::FunctionDecl) -> FnDef {
        let attributes = self.attributes(f.attributes());
        let params = f.params().map(|p| self.params(&p)).unwrap_or_default();
        let body = self.opt(f.body());
        FnDef {
            name: Some(f.name_token().map(|t| t.text().into()).unwrap_or_else(|| "".into())),
            doc: f.doc_comment(),
            attributes,
            params,
            body,
            namespace: self.namespace.clone(),
            unit: self.unit.clone(),
            range: Some(f.syntax().text_range()),
        }
    }

    fn params(&mut self, list: &ast::ParamList) -> Vec<Param> {
        list.params()
            .iter()
            .map(|p| {
                let (simple, attributes, default) = match p {
                    ast::Param::Simple(s) => (Some(s.clone()), vec![], None),
                    ast::Param::Paren(p) => {
                        let attributes = self.attributes(p.attributes());
                        let default = p.default().map(|d| self.expr(&d));
                        (p.param(), attributes, default)
                    }
                };
                let pattern = simple.as_ref().and_then(|s| s.pattern()).map(|p| self.pattern(&p));
                let name = match &pattern {
                    Some(Pattern::Ident(name)) => Some(name.clone()),
                    _ => None,
                };
                Param {
                    attributes,
                    name,
                    variadic: simple.as_ref().is_some_and(|s| s.is_variadic()),
                    default,
                    pattern,
                    range: Some(p.syntax().text_range()),
                }
            })
            .collect()
    }

    fn pattern(&mut self, p: &ast::Pattern) -> Pattern {
        match p {
            ast::Pattern::Ident(i) => Pattern::Ident(i.name().into()),
            ast::Pattern::Hole(_) => Pattern::Hole,
            ast::Pattern::Object(o) => Pattern::Object(
                o.entries()
                    .iter()
                    .map(|entry| match entry {
                        ast::ObjectPatternEntry::Full(f) => ObjectPatternEntry {
                            field: f.field().into(),
                            pattern: f.pattern().map(|p| self.pattern(&p)),
                        },
                        ast::ObjectPatternEntry::Shorthand(s) => ObjectPatternEntry {
                            field: s.field().into(),
                            pattern: None,
                        },
                    })
                    .collect(),
            ),
            ast::Pattern::List(l) => Pattern::List(l.items().iter().map(|p| self.pattern(p)).collect()),
        }
    }

    /// `import a.b.c` imports `c` from `a.b`; `import a.b._` imports everything from `a.b`
    fn import(&mut self, i: &ast::ImportStmt) -> ExprKind {
        let mut segments = i.segments();
        let name = match segments.pop() {
            Some(last) if last == "_" => None,
            Some(last) => Some(Rc::from(last)),
            None => return ExprKind::Missing,
        };
        let mut segments = segments.into_iter();
        let Some(first) = segments.next() else {
            return ExprKind::Missing;
        };
        let mut expr = self.node(None, ExprKind::Identifier(first.into()));
        for segment in segments {
            let kind = ExprKind::Member {
                target: expr,
                name: segment.into(),
                null_safe: false,
                name_range: None,
            };
            expr = self.node(None, kind);
        }
        ExprKind::Import { expr, name }
    }

    fn mish(&mut self, mish: &ast::MishExpr, capture_output: bool) -> E {
        let range = mish.syntax().text_range();
        let kind = self.mish_kind(mish, capture_output);
        self.node(Some(range), kind)
    }

    fn mish_kind(&mut self, mish: &ast::MishExpr, capture_output: bool) -> ExprKind {
        let mut words = vec![];
        let mut redirects = vec![];
        for item in mish.items() {
            match item {
                MishItem::Redirect(r) => {
                    let op = match r.op() {
                        Some(SyntaxKind::LESS_THAN) => RedirectOp::StandardInput,
                        _ => RedirectOp::StandardOutput,
                    };
                    let arg = match r.item() {
                        Some(item) => self.mish_item(item),
                        None => self.missing(),
                    };
                    redirects.push(MishRedirect { op, arg });
                }
                item => words.push(self.mish_item(item)),
            }
        }
        let mut words = words.into_iter();
        let command = words.next().unwrap_or_else(|| self.missing());
        ExprKind::Mish {
            command,
            args: words.collect(),
            redirects,
            capture_output,
        }
    }

    fn mish_item(&mut self, item: MishItem) -> E {
        match item {
            // Bare words are path strings
            MishItem::Word(t) => self.node(
                Some(t.text_range()),
                ExprKind::String {
                    value: t.text().into(),
                    quote: QuoteKind::Double,
                    tilde: false,
                },
            ),
            MishItem::Literal(l) => self.expr(&ast::Expr::Literal(l)),
            MishItem::String(s) => self.expr(&ast::Expr::InterpolatedString(s)),
            MishItem::Simple(s) => {
                let inner = self.opt(s.expr());
                self.node(Some(s.syntax().text_range()), ExprKind::MishInterpolation(inner))
            }
            MishItem::Complex(c) => {
                let inner = self.opt(c.expr());
                self.node(Some(c.syntax().text_range()), ExprKind::MishInterpolation(inner))
            }
            MishItem::Redirect(_) => self.missing(),
        }
    }
}
