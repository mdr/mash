//! Assignment and destructuring (Scala `AssignmentEvaluator`).

use crate::ast::{BinOp, E, ExprKind, Pattern};
use crate::value::{Object, Scope, Value};

use super::{Ctx, Location, MashError, R, render};

impl Ctx {
    pub(crate) fn assignment(&mut self, e: &E, lhs: &E, op: Option<BinOp>, rhs: &E, scope: &Scope) -> R<Value> {
        let right = self.eval(rhs, scope)?;
        match &lhs.kind {
            ExprKind::Identifier(name) => {
                let value = match op {
                    Some(op) => {
                        let current = self.lookup_identifier_value(name, lhs, scope)?;
                        self.binary_op(op, &current, &right, e.range)?
                    }
                    None => right,
                };
                scope.assign(name, value.clone());
                Ok(value)
            }
            ExprKind::Member {
                target,
                name,
                null_safe: false,
                ..
            } => match self.eval(target, scope)? {
                Value::Object(object) => self.assign_field(&object, Value::str(name), op, right, lhs, e),
                other => Err(MashError::at(
                    format!("Cannot assign to fields of a value of type {}", other.type_name()),
                    self.loc(e.range),
                )),
            },
            ExprKind::Lookup { target, index } => {
                let target_value = self.eval(target, scope)?;
                let index_value = self.eval(index, scope)?;
                match &target_value {
                    Value::List(xs) => {
                        let Value::Number(n, _) = &index_value else {
                            return Err(MashError::at(
                                format!("Invalid list index of type {}", index_value.type_name()),
                                self.loc(index.range),
                            ));
                        };
                        let i = n
                            .as_i64()
                            .ok_or_else(|| MashError::at(format!("Invalid list index '{n}'"), self.loc(index.range)))?;
                        if i < 0 || i >= xs.len() as i64 {
                            return Err(MashError::at(
                                format!("Index out of range '{n}'"),
                                self.loc(index.range),
                            ));
                        }
                        let value = match op {
                            Some(op) => {
                                let current = xs.get(i).unwrap_or(Value::Null);
                                self.binary_op(op, &current, &right, lhs.range)?
                            }
                            None => right,
                        };
                        xs.set(i, value.clone());
                        Ok(value)
                    }
                    Value::Object(object) => self.assign_field(object, index_value, op, right, lhs, e),
                    other => Err(MashError::at(
                        format!("Cannot assign to indexes of objects of type {}", other.type_name()),
                        self.loc(target.range),
                    )),
                }
            }
            _ => Err(MashError::at("Expression is not assignable", self.loc(lhs.range))),
        }
    }

    fn lookup_identifier_value(&mut self, name: &str, e: &E, scope: &Scope) -> R<Value> {
        // As `evaluateIdentifier`: the variable's value, without calling it
        Ok(self.lookup_identifier(name, e.range, scope)?.value)
    }

    fn assign_field(
        &mut self,
        object: &Object,
        field: Value,
        op: Option<BinOp>,
        right: Value,
        lhs: &E,
        e: &E,
    ) -> R<Value> {
        let value = match op {
            Some(op) => match object.get(&field) {
                Some(current) => self.binary_op(op, &current, &right, e.range)?,
                None => {
                    return Err(MashError::at(
                        format!("No field '{}' to update", render(&field)),
                        self.loc(lhs.range),
                    ));
                }
            },
            None => right,
        };
        object.set(field, value.clone());
        Ok(value)
    }

    /// Destructure `value` with `pattern`, calling `bind` for each name bound. Missing list elements and object
    /// fields are null.
    pub(crate) fn bind_pattern(
        &mut self,
        pattern: &Pattern,
        value: &Value,
        bind: &mut dyn FnMut(&str, Value),
        location: Option<Location>,
    ) -> R<()> {
        match pattern {
            Pattern::Ident(name) => bind(name, value.clone()),
            Pattern::Hole => {}
            Pattern::Object(entries) => {
                let Value::Object(object) = value else {
                    return Err(MashError::argument(
                        format!(
                            "Cannot match object pattern against value of type {}",
                            value.type_name()
                        ),
                        location,
                    ));
                };
                for entry in entries {
                    let field = object.get_str(&entry.field).unwrap_or(Value::Null);
                    match &entry.pattern {
                        Some(pattern) => self.bind_pattern(pattern, &field, bind, location.clone())?,
                        None => bind(&entry.field, field),
                    }
                }
            }
            Pattern::List(patterns) => {
                let Value::List(xs) = value else {
                    return Err(MashError::argument(
                        format!("Cannot match list pattern against value of type {}", value.type_name()),
                        location,
                    ));
                };
                let items = xs.snapshot();
                for (i, pattern) in patterns.iter().enumerate() {
                    let item = items.get(i).cloned().unwrap_or(Value::Null);
                    self.bind_pattern(pattern, &item, bind, location.clone())?;
                }
            }
        }
        Ok(())
    }
}
