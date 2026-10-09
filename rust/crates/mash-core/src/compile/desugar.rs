//! Desugaring passes: headless members, holes, pipes (and removing parentheses, used by tests). Ports of
//! `AddHolesToHeadlessMembers`, `DesugarHoles`, `DesugarPipes` and `ParenRemover`.

use std::rc::Rc;

use crate::ast::*;

/// The variables that holes (`_`, `_1`, …) become
pub const HOLE_VARIABLE_PREFIX: &str = "$hole";

fn hole_variable(n: u8) -> Rc<str> {
    format!("{HOLE_VARIABLE_PREFIX}{n}").into()
}

/// `.foo` means `_.foo`
pub fn add_holes_to_headless_members(e: &E, ids: &mut NodeIdGen) -> E {
    transform(e, &mut |e| match &e.kind {
        ExprKind::HeadlessMember {
            name,
            null_safe,
            name_range,
        } => {
            let hole = Expr::new(ids, None, ExprKind::Hole(1));
            e.with_kind(ExprKind::Member {
                target: hole,
                name: name.clone(),
                null_safe: *null_safe,
                name_range: *name_range,
            })
        }
        _ => e,
    })
}

/// `a | f b` means `f b a`; `a | f` means `f a`
pub fn desugar_pipes(e: &E) -> E {
    transform(e, &mut |e| match &e.kind {
        ExprKind::Pipe { lhs, rhs } => match &rhs.kind {
            ExprKind::Invocation { function, args, .. } => {
                let mut args = args.clone();
                args.push(Arg::Positional(lhs.clone()));
                e.with_kind(ExprKind::Invocation {
                    function: function.clone(),
                    args,
                    paren: false,
                })
            }
            _ => e.with_kind(ExprKind::Invocation {
                function: rhs.clone(),
                args: vec![Arg::Positional(lhs.clone())],
                paren: false,
            }),
        },
        _ => e,
    })
}

pub fn remove_parens(e: &E) -> E {
    transform(e, &mut |e| match &e.kind {
        ExprKind::Paren(inner) => inner.clone(),
        _ => e,
    })
}

/// Turn expressions containing holes into lambdas: `_.size > 10` becomes `$hole1 => $hole1.size > 10`. The
/// lambda goes around the smallest enclosing "boundary": parentheses, a block, a lambda or function body, a
/// pipeline stage, or an argument of a parenthesised call.
pub fn desugar_holes(e: &E, ids: &mut NodeIdGen, unit: &Rc<SourceUnit>) -> E {
    HoleDesugarer { ids, unit }.add_lambda_if_needed(e)
}

struct HoleDesugarer<'a> {
    ids: &'a mut NodeIdGen,
    unit: &'a Rc<SourceUnit>,
}

impl HoleDesugarer<'_> {
    fn add_lambda_if_needed(&mut self, e: &E) -> E {
        let (body, holes) = self.desugar(e);
        if holes == 0 {
            return body;
        }
        let params = (1..=holes)
            .map(|n| {
                let name = hole_variable(n);
                Param {
                    attributes: vec![],
                    name: Some(name.clone()),
                    variadic: false,
                    default: None,
                    pattern: Some(Pattern::Ident(name)),
                    range: None,
                }
            })
            .collect();
        let def = FnDef {
            name: None,
            doc: None,
            attributes: vec![],
            params,
            body,
            namespace: None,
            unit: self.unit.clone(),
            range: e.range,
        };
        Expr::new(self.ids, None, ExprKind::Lambda(Rc::new(def)))
    }

    /// The desugared expression, and the highest hole number in it that still needs a lambda
    fn desugar(&mut self, e: &E) -> (E, u8) {
        use ExprKind as K;
        let mut max = 0;
        // Children that propagate holes outwards
        let mut inner = |this: &mut Self, child: &E| {
            let (child, holes) = this.desugar(child);
            max = max.max(holes);
            child
        };
        let kind = match &e.kind {
            K::Hole(n) => return (e.with_kind(K::Identifier(hole_variable(*n))), *n),
            K::Literal(_)
            | K::String { .. }
            | K::Identifier(_)
            | K::MishFunction(_)
            | K::HeadlessMember { .. }
            | K::This
            | K::Missing => return (e.clone(), 0),
            // Boundaries
            K::Lambda(def) => K::Lambda(Rc::new(self.fn_def(def))),
            K::FunctionDecl(def) => K::FunctionDecl(Rc::new(self.fn_def(def))),
            K::ClassDecl(def) => K::ClassDecl(Rc::new(ClassDef {
                params: self.params(&def.params),
                methods: def.methods.iter().map(|m| Rc::new(self.fn_def(m))).collect(),
                ..(**def).clone()
            })),
            K::Paren(e) => K::Paren(self.add_lambda_if_needed(e)),
            K::Block(e) => K::Block(self.add_lambda_if_needed(e)),
            K::Pipe { lhs, rhs } => K::Pipe {
                lhs: inner(self, lhs),
                rhs: self.add_lambda_if_needed(rhs),
            },
            K::Invocation {
                function,
                args,
                paren: true,
            } => K::Invocation {
                function: inner(self, function),
                args: args
                    .iter()
                    .filter_map(|a| match a {
                        Arg::Positional(e) => Some(Arg::Positional(self.add_lambda_if_needed(e))),
                        _ => None,
                    })
                    .collect(),
                paren: true,
            },
            // Everything else propagates holes outwards
            K::Invocation {
                function,
                args,
                paren: false,
            } => K::Invocation {
                function: inner(self, function),
                args: args
                    .iter()
                    .map(|a| match a {
                        Arg::Positional(e) => Arg::Positional(inner(self, e)),
                        Arg::ShortFlags { .. } => a.clone(),
                        Arg::LongFlag { name, value, range } => Arg::LongFlag {
                            name: name.clone(),
                            value: value.as_ref().map(|v| inner(self, v)),
                            range: *range,
                        },
                    })
                    .collect(),
                paren: false,
            },
            K::Interpolated { parts, tilde } => K::Interpolated {
                parts: parts
                    .iter()
                    .map(|p| match p {
                        InterpolationPart::Text(t) => InterpolationPart::Text(t.clone()),
                        InterpolationPart::Expr(e) => InterpolationPart::Expr(inner(self, e)),
                    })
                    .collect(),
                tilde: *tilde,
            },
            K::StatementSeq(es) => K::StatementSeq(es.iter().map(|e| inner(self, e)).collect()),
            K::List(es) => K::List(es.iter().map(|e| inner(self, e)).collect()),
            K::Object(entries) => K::Object(
                entries
                    .iter()
                    .map(|entry| match entry {
                        ObjectEntry::Full { field, value } => ObjectEntry::Full {
                            field: inner(self, field),
                            value: inner(self, value),
                        },
                        shorthand => shorthand.clone(),
                    })
                    .collect(),
            ),
            K::Member {
                target,
                name,
                null_safe,
                name_range,
            } => K::Member {
                target: inner(self, target),
                name: name.clone(),
                null_safe: *null_safe,
                name_range: *name_range,
            },
            K::Lookup { target, index } => K::Lookup {
                target: inner(self, target),
                index: inner(self, index),
            },
            K::Minus(e) => K::Minus(inner(self, e)),
            K::BinOp { op, lhs, rhs } => K::BinOp {
                op: *op,
                lhs: inner(self, lhs),
                rhs: inner(self, rhs),
            },
            K::ChainedOp { first, rest } => K::ChainedOp {
                first: inner(self, first),
                rest: rest.iter().map(|(op, e)| (*op, inner(self, e))).collect(),
            },
            K::If { cond, then, else_ } => K::If {
                cond: inner(self, cond),
                then: inner(self, then),
                else_: else_.as_ref().map(|e| inner(self, e)),
            },
            K::Assignment { lhs, op, rhs } => K::Assignment {
                lhs: inner(self, lhs),
                op: *op,
                rhs: inner(self, rhs),
            },
            K::PatternAssignment { pattern, rhs } => K::PatternAssignment {
                pattern: pattern.clone(),
                rhs: inner(self, rhs),
            },
            K::Mish {
                command,
                args,
                redirects,
                capture_output,
            } => K::Mish {
                command: inner(self, command),
                args: args.iter().map(|e| inner(self, e)).collect(),
                redirects: redirects
                    .iter()
                    .map(|r| MishRedirect {
                        op: r.op,
                        arg: inner(self, &r.arg),
                    })
                    .collect(),
                capture_output: *capture_output,
            },
            K::MishInterpolation(e) => K::MishInterpolation(inner(self, e)),
            K::Help(e) => K::Help(inner(self, e)),
            K::Import { expr, name, name_range } => K::Import {
                expr: inner(self, expr),
                name: name.clone(),
                name_range: *name_range,
            },
        };
        (e.with_kind(kind), max)
    }

    fn params(&mut self, params: &[Param]) -> Vec<Param> {
        params
            .iter()
            .map(|p| Param {
                default: p.default.as_ref().map(|d| self.add_lambda_if_needed(d)),
                ..p.clone()
            })
            .collect()
    }

    fn fn_def(&mut self, def: &FnDef) -> FnDef {
        FnDef {
            params: self.params(&def.params),
            body: self.add_lambda_if_needed(&def.body),
            ..def.clone()
        }
    }
}
