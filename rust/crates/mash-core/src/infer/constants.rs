//! Statically known values of expressions (Scala `SimpleEvaluator`): literals, lists and objects of constants,
//! global variables, and simple field and index lookups on those. Used by type inference (e.g. the field
//! names given to `select`) and completion.

use std::collections::HashMap;

use crate::ast::*;
use crate::builtins::os::PATH_CLASS;
use crate::compile::HOLE_VARIABLE_PREFIX;
use crate::value::{List, Object, Scope, Str, Value};

pub(crate) struct ConstantFolder<'a> {
    pub(crate) globals: &'a Scope,
    pub(crate) home: String,
    pub(crate) constants: HashMap<NodeId, Value>,
}

impl ConstantFolder<'_> {
    /// Record the constant value of `e` and its subexpressions; returns `e`'s
    pub(crate) fn fold(&mut self, e: &E) -> Option<Value> {
        let value = self.compute(e);
        if let Some(v) = &value {
            self.constants.insert(e.id, v.clone());
        }
        value
    }

    fn fold_all(&mut self, e: &E) {
        e.for_each_child(&mut |child| {
            self.fold(child);
        });
    }

    fn compute(&mut self, e: &E) -> Option<Value> {
        use ExprKind as K;
        match &e.kind {
            K::Literal(Literal::Number(n)) => Some(Value::num(*n)),
            K::Literal(Literal::Bool(b)) => Some(Value::Bool(*b)),
            K::Literal(Literal::Null) => Some(Value::Null),
            K::String { value, quote, tilde } => {
                let text = if *tilde {
                    format!("{}{value}", self.home)
                } else {
                    value.to_string()
                };
                Some(match quote {
                    QuoteKind::Double => Value::Str(Str::tagged(text, &PATH_CLASS)),
                    QuoteKind::Single => Value::Str(Str::new(text)),
                })
            }
            K::List(items) => {
                let values: Vec<Option<Value>> = items.iter().map(|item| self.fold(item)).collect();
                values
                    .into_iter()
                    .collect::<Option<Vec<_>>>()
                    .map(|vs| Value::List(List::new(vs)))
            }
            K::Identifier(name) => self.globals.lookup(name),
            K::Object(entries) => {
                let mut fields = vec![];
                let mut complete = true;
                for entry in entries {
                    match entry {
                        ObjectEntry::Full { field, value } => {
                            let name = match &field.kind {
                                K::Identifier(name) if !name.starts_with(HOLE_VARIABLE_PREFIX) => {
                                    Some(name.to_string())
                                }
                                _ => self.fold(field).and_then(|v| v.as_str().map(str::to_string)),
                            };
                            match (name, self.fold(value)) {
                                (Some(name), Some(value)) => fields.push((name, value)),
                                _ => complete = false,
                            }
                        }
                        ObjectEntry::Shorthand { name, .. } => match self.globals.lookup(name) {
                            Some(value) => fields.push((name.to_string(), value)),
                            None => complete = false,
                        },
                    }
                }
                complete.then(|| {
                    let object = Object::new();
                    for (k, v) in fields {
                        object.set_str(&k, v);
                    }
                    Value::Object(object)
                })
            }
            K::StatementSeq(statements) => {
                let mut last = Some(Value::Unit);
                for statement in statements {
                    last = self.fold(statement);
                }
                last
            }
            K::Paren(inner) | K::Block(inner) => self.fold(inner),
            K::Import { expr, .. } => self.fold(expr),
            K::Member { target, name, .. } => match self.fold(target)? {
                Value::Object(object) => object.get_str(name),
                _ => None,
            },
            K::Lookup { target, index } => {
                let index = self.fold(index);
                let target = self.fold(target);
                match (target?, index?) {
                    (Value::List(xs), Value::Number(n, _)) => xs.get(n.as_i64().filter(|i| *i >= 0)?),
                    _ => None,
                }
            }
            _ => {
                self.fold_all(e);
                None
            }
        }
    }
}
