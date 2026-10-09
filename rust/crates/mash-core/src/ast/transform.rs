//! Generic rebuilding of trees, for passes that rewrite particular kinds of node.

use std::rc::Rc;

use super::*;

impl Expr {
    /// This node with each direct child expression replaced by `f(child)`, keeping the node's id and range.
    /// Children include function bodies, parameter defaults, attribute arguments and class methods.
    pub fn map_children(&self, f: &mut dyn FnMut(&E) -> E) -> E {
        use ExprKind as K;
        let kind = match &self.kind {
            K::Literal(_)
            | K::String { .. }
            | K::Identifier(_)
            | K::Hole(_)
            | K::HeadlessMember { .. }
            | K::This
            | K::MishFunction(_)
            | K::Missing => self.kind.clone(),
            K::Interpolated { parts, tilde } => K::Interpolated {
                parts: map_parts(parts, f),
                tilde: *tilde,
            },
            K::Paren(e) => K::Paren(f(e)),
            K::Block(e) => K::Block(f(e)),
            K::StatementSeq(es) => K::StatementSeq(es.iter().map(&mut *f).collect()),
            K::List(es) => K::List(es.iter().map(&mut *f).collect()),
            K::Object(entries) => K::Object(
                entries
                    .iter()
                    .map(|entry| match entry {
                        ObjectEntry::Full { field, value } => ObjectEntry::Full {
                            field: f(field),
                            value: f(value),
                        },
                        shorthand @ ObjectEntry::Shorthand { .. } => shorthand.clone(),
                    })
                    .collect(),
            ),
            K::Member {
                target,
                name,
                null_safe,
                name_range,
            } => K::Member {
                target: f(target),
                name: name.clone(),
                null_safe: *null_safe,
                name_range: *name_range,
            },
            K::Lookup { target, index } => K::Lookup {
                target: f(target),
                index: f(index),
            },
            K::Invocation { function, args, paren } => K::Invocation {
                function: f(function),
                args: map_args(args, f),
                paren: *paren,
            },
            K::Minus(e) => K::Minus(f(e)),
            K::BinOp { op, lhs, rhs } => K::BinOp {
                op: *op,
                lhs: f(lhs),
                rhs: f(rhs),
            },
            K::ChainedOp { first, rest } => K::ChainedOp {
                first: f(first),
                rest: rest.iter().map(|(op, e)| (*op, f(e))).collect(),
            },
            K::If { cond, then, else_ } => K::If {
                cond: f(cond),
                then: f(then),
                else_: else_.as_ref().map(&mut *f),
            },
            K::Assignment { lhs, op, rhs } => K::Assignment {
                lhs: f(lhs),
                op: *op,
                rhs: f(rhs),
            },
            K::PatternAssignment { pattern, rhs } => K::PatternAssignment {
                pattern: pattern.clone(),
                rhs: f(rhs),
            },
            K::Lambda(def) => K::Lambda(Rc::new(def.map_exprs(f))),
            K::FunctionDecl(def) => K::FunctionDecl(Rc::new(def.map_exprs(f))),
            K::ClassDecl(def) => K::ClassDecl(Rc::new(def.map_exprs(f))),
            K::Pipe { lhs, rhs } => K::Pipe {
                lhs: f(lhs),
                rhs: f(rhs),
            },
            K::Mish {
                command,
                args,
                redirects,
                capture_output,
            } => K::Mish {
                command: f(command),
                args: args.iter().map(&mut *f).collect(),
                redirects: redirects
                    .iter()
                    .map(|r| MishRedirect {
                        op: r.op,
                        arg: f(&r.arg),
                    })
                    .collect(),
                capture_output: *capture_output,
            },
            K::MishInterpolation(e) => K::MishInterpolation(f(e)),
            K::Help(e) => K::Help(f(e)),
            K::Import { expr, name } => K::Import {
                expr: f(expr),
                name: name.clone(),
            },
        };
        self.with_kind(kind)
    }
}

pub(crate) fn map_parts(parts: &[InterpolationPart], f: &mut dyn FnMut(&E) -> E) -> Vec<InterpolationPart> {
    parts
        .iter()
        .map(|part| match part {
            InterpolationPart::Text(t) => InterpolationPart::Text(t.clone()),
            InterpolationPart::Expr(e) => InterpolationPart::Expr(f(e)),
        })
        .collect()
}

pub(crate) fn map_args(args: &[Arg], f: &mut dyn FnMut(&E) -> E) -> Vec<Arg> {
    args.iter()
        .map(|arg| match arg {
            Arg::Positional(e) => Arg::Positional(f(e)),
            Arg::ShortFlags { .. } => arg.clone(),
            Arg::LongFlag { name, value, range } => Arg::LongFlag {
                name: name.clone(),
                value: value.as_ref().map(&mut *f),
                range: *range,
            },
        })
        .collect()
}

pub(crate) fn map_attributes(attributes: &[Attribute], f: &mut dyn FnMut(&E) -> E) -> Vec<Attribute> {
    attributes
        .iter()
        .map(|a| Attribute {
            name: a.name.clone(),
            args: a.args.as_ref().map(|args| map_args(args, f)),
            range: a.range,
        })
        .collect()
}

pub(crate) fn map_params(params: &[Param], f: &mut dyn FnMut(&E) -> E) -> Vec<Param> {
    params
        .iter()
        .map(|p| Param {
            attributes: map_attributes(&p.attributes, f),
            default: p.default.as_ref().map(&mut *f),
            ..p.clone()
        })
        .collect()
}

impl FnDef {
    /// This definition with each expression in it (parameter defaults, attribute arguments, body) replaced by
    /// `f(expr)`
    pub fn map_exprs(&self, f: &mut dyn FnMut(&E) -> E) -> FnDef {
        FnDef {
            attributes: map_attributes(&self.attributes, f),
            params: map_params(&self.params, f),
            body: f(&self.body),
            ..self.clone()
        }
    }
}

impl ClassDef {
    pub fn map_exprs(&self, f: &mut dyn FnMut(&E) -> E) -> ClassDef {
        ClassDef {
            attributes: map_attributes(&self.attributes, f),
            params: map_params(&self.params, f),
            methods: self.methods.iter().map(|m| Rc::new(m.map_exprs(f))).collect(),
            ..self.clone()
        }
    }
}

/// Rewrite a tree bottom-up: children first, then `f` on the rebuilt node (Scala `AstNode.transform`).
pub fn transform(e: &E, f: &mut dyn FnMut(E) -> E) -> E {
    let rebuilt = e.map_children(&mut |child| transform(child, f));
    f(rebuilt)
}

impl Expr {
    /// Call `f` on each direct child expression (as [`Expr::map_children`] would), without rebuilding.
    pub fn for_each_child(&self, f: &mut dyn FnMut(&E)) {
        use ExprKind as K;
        let attributes = |attributes: &[Attribute], f: &mut dyn FnMut(&E)| {
            for a in attributes {
                a.args.iter().flatten().for_each(|arg| arg_exprs(arg, f));
            }
        };
        let params = |params: &[Param], f: &mut dyn FnMut(&E)| {
            for p in params {
                attributes(&p.attributes, f);
                p.default.iter().for_each(&mut *f);
            }
        };
        let fn_def = |def: &FnDef, f: &mut dyn FnMut(&E)| {
            attributes(&def.attributes, f);
            params(&def.params, f);
            f(&def.body);
        };
        match &self.kind {
            K::Literal(_)
            | K::String { .. }
            | K::Identifier(_)
            | K::Hole(_)
            | K::HeadlessMember { .. }
            | K::This
            | K::MishFunction(_)
            | K::Missing => {}
            K::Interpolated { parts, .. } => {
                for part in parts {
                    if let InterpolationPart::Expr(e) = part {
                        f(e)
                    }
                }
            }
            K::Paren(e) | K::Block(e) | K::Minus(e) | K::MishInterpolation(e) | K::Help(e) => f(e),
            K::Import { expr, .. } => f(expr),
            K::StatementSeq(es) | K::List(es) => es.iter().for_each(f),
            K::Object(entries) => {
                for entry in entries {
                    if let ObjectEntry::Full { field, value } = entry {
                        f(field);
                        f(value);
                    }
                }
            }
            K::Member { target, .. } => f(target),
            K::Lookup { target, index } => {
                f(target);
                f(index)
            }
            K::Invocation { function, args, .. } => {
                f(function);
                args.iter().for_each(|arg| arg_exprs(arg, f));
            }
            K::BinOp { lhs, rhs, .. } | K::Assignment { lhs, rhs, .. } | K::Pipe { lhs, rhs } => {
                f(lhs);
                f(rhs)
            }
            K::ChainedOp { first, rest } => {
                f(first);
                rest.iter().for_each(|(_, e)| f(e));
            }
            K::If { cond, then, else_ } => {
                f(cond);
                f(then);
                else_.iter().for_each(f);
            }
            K::PatternAssignment { rhs, .. } => f(rhs),
            K::Lambda(def) | K::FunctionDecl(def) => fn_def(def, f),
            K::ClassDecl(def) => {
                attributes(&def.attributes, f);
                params(&def.params, f);
                def.methods.iter().for_each(|m| fn_def(m, f));
            }
            K::Mish {
                command,
                args,
                redirects,
                ..
            } => {
                f(command);
                args.iter().for_each(&mut *f);
                redirects.iter().for_each(|r| f(&r.arg));
            }
        }
    }
}

fn arg_exprs(arg: &Arg, f: &mut dyn FnMut(&E)) {
    match arg {
        Arg::Positional(e) | Arg::LongFlag { value: Some(e), .. } => f(e),
        _ => {}
    }
}
