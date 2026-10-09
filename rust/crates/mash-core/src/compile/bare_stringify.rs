//! Bare words: identifiers that aren't bound to anything become path strings, so `cd foo` works without
//! quotes. Port of `BareStringify`.

use std::collections::HashSet;
use std::rc::Rc;

use mash_syntax::TextRange;

use crate::ast::*;

/// Replace unbound identifiers in `e` with double-quoted string literals. `bindings` are the names already
/// bound (globals). Also returns the ranges of the identifiers that were replaced (for highlighting).
pub fn bare_stringify(e: &E, bindings: &HashSet<Rc<str>>) -> (E, Vec<TextRange>) {
    let mut stringifier = BareStringifier { bare_words: vec![] };
    let result = stringifier.expr(e, &Bindings::Global(bindings));
    (result, stringifier.bare_words)
}

/// A chain of name sets, innermost first
enum Bindings<'a> {
    Global(&'a HashSet<Rc<str>>),
    Local(Vec<Rc<str>>, &'a Bindings<'a>),
}

impl Bindings<'_> {
    fn contains(&self, name: &str) -> bool {
        match self {
            Bindings::Global(names) => names.contains(name),
            Bindings::Local(names, outer) => names.iter().any(|n| &**n == name) || outer.contains(name),
        }
    }

    fn with(&self, names: Vec<Rc<str>>) -> Bindings<'_> {
        Bindings::Local(names, self)
    }
}

struct BareStringifier {
    bare_words: Vec<TextRange>,
}

impl BareStringifier {
    fn expr(&mut self, e: &E, bindings: &Bindings) -> E {
        use ExprKind as K;
        match &e.kind {
            K::Identifier(name) => {
                if bindings.contains(name) {
                    e.clone()
                } else {
                    self.bare_words.extend(e.range);
                    e.with_kind(K::String {
                        value: name.clone(),
                        quote: QuoteKind::Double,
                        tilde: false,
                    })
                }
            }
            K::StatementSeq(statements) => {
                let mut names = vec![];
                let mut result = vec![];
                for statement in statements {
                    result.push(self.expr(statement, &bindings.with(names.clone())));
                    names.extend(newly_bound_names(statement));
                }
                e.with_kind(K::StatementSeq(result))
            }
            // The target of a plain assignment to a name isn't a reference to it
            K::Assignment { lhs, op, rhs } if matches!(lhs.kind, K::Identifier(_)) => e.with_kind(K::Assignment {
                lhs: lhs.clone(),
                op: *op,
                rhs: self.expr(rhs, bindings),
            }),
            K::Object(entries) => e.with_kind(K::Object(
                entries
                    .iter()
                    .map(|entry| match entry {
                        // A plain identifier field is a name, not a reference
                        ObjectEntry::Full { field, value } if matches!(field.kind, K::Identifier(_)) => {
                            ObjectEntry::Full {
                                field: field.clone(),
                                value: self.expr(value, bindings),
                            }
                        }
                        ObjectEntry::Full { field, value } => ObjectEntry::Full {
                            field: self.expr(field, bindings),
                            value: self.expr(value, bindings),
                        },
                        shorthand => shorthand.clone(),
                    })
                    .collect(),
            )),
            K::Lambda(def) => {
                let names = def.params.iter().flat_map(Param::bound_names).collect();
                e.with_kind(K::Lambda(Rc::new(self.fn_def(def, bindings, names))))
            }
            K::FunctionDecl(def) => {
                let mut names: Vec<Rc<str>> = def.params.iter().flat_map(Param::bound_names).collect();
                names.extend(def.name.clone());
                e.with_kind(K::FunctionDecl(Rc::new(self.fn_def(def, bindings, names))))
            }
            K::ClassDecl(def) => {
                let mut body_names: Vec<Rc<str>> = def.params.iter().flat_map(Param::bound_names).collect();
                body_names.extend(def.methods.iter().filter_map(|m| m.name.clone()));
                let body_bindings = bindings.with(body_names);
                let methods = def
                    .methods
                    .iter()
                    .map(|m| {
                        let mut names: Vec<Rc<str>> = m.params.iter().flat_map(Param::bound_names).collect();
                        names.extend(m.name.clone());
                        Rc::new(self.fn_def(m, &body_bindings, names))
                    })
                    .collect();
                e.with_kind(K::ClassDecl(Rc::new(ClassDef {
                    attributes: self.attributes(&def.attributes, bindings),
                    params: self.params(&def.params, bindings),
                    methods,
                    ..(**def).clone()
                })))
            }
            // Everything else: the same bindings apply throughout
            _ => e.map_children(&mut |child| self.expr(child, bindings)),
        }
    }

    /// Attribute arguments and parameter defaults see the enclosing bindings; the body also sees `names`
    fn fn_def(&mut self, def: &FnDef, bindings: &Bindings, names: Vec<Rc<str>>) -> FnDef {
        FnDef {
            attributes: self.attributes(&def.attributes, bindings),
            params: self.params(&def.params, bindings),
            body: self.expr(&def.body, &bindings.with(names)),
            ..def.clone()
        }
    }

    fn attributes(&mut self, attributes: &[Attribute], bindings: &Bindings) -> Vec<Attribute> {
        transform::map_attributes(attributes, &mut |e| self.expr(e, bindings))
    }

    fn params(&mut self, params: &[Param], bindings: &Bindings) -> Vec<Param> {
        transform::map_params(params, &mut |e| self.expr(e, bindings))
    }
}

/// Names bound by assignments and declarations anywhere in `e`
fn newly_bound_names(e: &E) -> Vec<Rc<str>> {
    let mut names = vec![];
    collect_bound_names(e, &mut names);
    names
}

fn collect_bound_names(e: &E, names: &mut Vec<Rc<str>>) {
    match &e.kind {
        ExprKind::Assignment { lhs, .. } => {
            if let ExprKind::Identifier(name) = &lhs.kind {
                names.push(name.clone());
            }
        }
        ExprKind::PatternAssignment { pattern, .. } => names.extend(pattern.bound_names()),
        ExprKind::FunctionDecl(def) => names.extend(def.name.clone()),
        ExprKind::ClassDecl(def) => names.push(def.name.clone()),
        _ => {}
    }
    e.for_each_child(&mut |child| collect_bound_names(child, names));
}
