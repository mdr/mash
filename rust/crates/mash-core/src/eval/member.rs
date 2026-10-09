//! Member lookup: fields, methods (including inherited and tag-class methods), static methods, and
//! vectorised lookup over lists (Scala `MemberEvaluator`).

use crate::builtins::core::*;
use crate::builtins::{collections::LIST_CLASS, time};
use crate::value::{BoundMethod, BuiltinClass, ClassRef, Function, FunctionKind, MethodRef, Value};

use super::{Ctx, Location, MashError, R, suggestion_suffix};

/// Which members a lookup can see.
#[derive(Clone, Copy, Debug)]
pub struct MemberLookup {
    /// Private methods (only via `this`)
    pub include_private: bool,
    /// Shy methods (not seen by unqualified names inside methods, nor imported)
    pub include_shy: bool,
}

impl MemberLookup {
    pub const DEFAULT: MemberLookup = MemberLookup {
        include_private: false,
        include_shy: true,
    };
}

/// The result of evaluating a member expression.
#[derive(Debug)]
pub(crate) struct MemberResult {
    pub value: Value,
    /// The target was a list, and the member was looked up in each element
    pub vectorised: bool,
}

/// The builtin class of a value that isn't an object or class
pub(crate) fn builtin_class_of(value: &Value) -> &'static BuiltinClass {
    match value {
        Value::Null => &NULL_CLASS,
        Value::Unit => &UNIT_CLASS,
        Value::Bool(_) => &BOOLEAN_CLASS,
        Value::Number(..) => &NUMBER_CLASS,
        Value::Str(_) => &STRING_CLASS,
        Value::List(_) => &LIST_CLASS,
        Value::Object(_) => &OBJECT_CLASS,
        Value::Function(_) | Value::Builtin(_) => &FUNCTION_CLASS,
        Value::BoundMethod(_) => &BOUND_METHOD_CLASS,
        Value::Class(_) => &CLASS_CLASS,
        Value::DateTime(_) => &time::DATE_TIME_CLASS,
        Value::Date(_) => &time::DATE_CLASS,
    }
}

/// The class of a value (Scala `primaryClass`)
pub fn primary_class(value: &Value) -> ClassRef {
    match value {
        Value::Object(o) => o.class().unwrap_or(ClassRef::Builtin(&OBJECT_CLASS)),
        _ => ClassRef::Builtin(builtin_class_of(value)),
    }
}

/// Whether a value is an instance of a class, including via its tag (Scala `isA`)
pub fn is_a(value: &Value, class: &ClassRef) -> bool {
    is_subclass_of(&primary_class(value), class)
        || value
            .tag()
            .is_some_and(|tag| is_subclass_of(&ClassRef::Builtin(tag), class))
}

pub fn is_subclass_of(class: &ClassRef, other: &ClassRef) -> bool {
    let mut current = Some(class.clone());
    while let Some(c) = current {
        if c.ptr_eq(other) {
            return true;
        }
        current = parent_of(&c);
    }
    false
}

pub fn parent_of(class: &ClassRef) -> Option<ClassRef> {
    match class {
        ClassRef::Builtin(c) => c.parent.map(ClassRef::Builtin),
        // User classes extend Object
        ClassRef::User(_) => Some(ClassRef::Builtin(&OBJECT_CLASS)),
    }
}

impl Ctx {
    /// A field, method bound to the target, or static method (Scala `maybeLookupByString`).
    pub fn lookup_member(&self, target: &Value, name: &str, lookup: MemberLookup) -> Option<Value> {
        let in_class = |class: ClassRef| lookup_in_class(target, &class, name, lookup);
        let builtin = |class: &'static BuiltinClass| in_class(ClassRef::Builtin(class));
        match target {
            Value::Number(_, tag) => builtin(&NUMBER_CLASS).or_else(|| tag.and_then(builtin)),
            Value::Str(s) => builtin(&STRING_CLASS).or_else(|| s.tag().and_then(builtin)),
            Value::Class(class) => static_method(class, name).or_else(|| builtin(&CLASS_CLASS)),
            Value::Object(obj) => obj.get_str(name).or_else(|| {
                let class = obj.class().unwrap_or(ClassRef::Builtin(&OBJECT_CLASS));
                lookup_in_class(target, &class, name, lookup)
            }),
            _ => builtin(builtin_class_of(target)),
        }
    }

    /// Names of a value's members (fields and public methods)
    pub fn member_names(&self, target: &Value, include_private: bool) -> Vec<String> {
        let mut names: Vec<String> = vec![];
        if let Value::Object(obj) = target {
            names.extend(obj.keys().iter().filter_map(|k| k.as_str().map(str::to_string)));
        }
        let mut classes = vec![primary_class(target)];
        if let Some(tag) = target.tag() {
            classes.push(ClassRef::Builtin(tag));
        }
        if let Value::Class(_) = target {
            classes = vec![ClassRef::Builtin(&CLASS_CLASS)];
        }
        for class in classes {
            let mut current = Some(class);
            while let Some(class) = current {
                names.extend(class_member_names(&class, include_private));
                current = parent_of(&class);
            }
        }
        let mut seen = std::collections::HashSet::new();
        names.retain(|n| seen.insert(n.clone()));
        names
    }

    /// Evaluate `target.name` (Scala `MemberEvaluator.evaluateMember`): look the member up in the target; if
    /// the target is a list and that fails, look it up in each element (vectorising).
    pub(crate) fn member(
        &mut self,
        target: &Value,
        name: &str,
        null_safe: bool,
        invoke_nullary_when_vectorising: bool,
        this_target: bool,
        location: Option<Location>,
    ) -> R<MemberResult> {
        let lookup = MemberLookup {
            include_private: this_target,
            include_shy: true,
        };
        if let Some(value) = self.lookup_member(target, name, lookup) {
            return Ok(MemberResult {
                value,
                vectorised: false,
            });
        }
        if let Value::List(xs) = target
            && let Some(values) = self.vectorised_member(
                &xs.snapshot(),
                name,
                null_safe,
                invoke_nullary_when_vectorising,
                &location,
            )?
        {
            return Ok(MemberResult {
                value: Value::list(values),
                vectorised: true,
            });
        }
        if null_safe {
            Ok(MemberResult {
                value: Value::Null,
                vectorised: false,
            })
        } else {
            Err(self.member_not_found(target, name, location))
        }
    }

    fn vectorised_member(
        &mut self,
        items: &[Value],
        name: &str,
        null_safe: bool,
        invoke_nullary: bool,
        location: &Option<Location>,
    ) -> R<Option<Vec<Value>>> {
        let mut values = Vec::with_capacity(items.len());
        for item in items {
            match self.lookup_member(item, name, MemberLookup::DEFAULT) {
                Some(value) => {
                    let value = if invoke_nullary {
                        self.invoke_nullary(value, location.clone())?
                    } else {
                        value
                    };
                    values.push(value)
                }
                None if null_safe => values.push(Value::Null),
                None => return Ok(None),
            }
        }
        Ok(Some(values))
    }

    pub(crate) fn member_not_found(&self, target: &Value, name: &str, location: Option<Location>) -> MashError {
        let names = self.member_names(target, false);
        let message = format!(
            "Cannot find member '{name}' in value of type {}{}",
            target.type_name(),
            suggestion_suffix(&names, name)
        );
        MashError::at(message, location)
    }
}

fn static_method(class: &ClassRef, name: &str) -> Option<Value> {
    match class {
        ClassRef::Builtin(c) => c.static_method(name).map(|f| Value::Builtin(crate::value::Builtin(f))),
        ClassRef::User(c) if name == "new" => {
            Some(Value::Function(Function::new(FunctionKind::Constructor(c.clone()))))
        }
        ClassRef::User(_) => None,
    }
}

/// A method of `class` or its ancestors, bound to `target` (Scala `maybeLookupInClass`)
fn lookup_in_class(target: &Value, class: &ClassRef, name: &str, lookup: MemberLookup) -> Option<Value> {
    let direct = match class {
        ClassRef::Builtin(c) => c
            .method(name)
            .filter(|m| !m.private() || lookup.include_private)
            .map(|method| (MethodRef::Builtin { method, class: c }, method.shy())),
        ClassRef::User(c) => c
            .find_method(name)
            .filter(|&i| !c.methods()[i].private || lookup.include_private)
            .map(|index| {
                (
                    MethodRef::User {
                        class: c.clone(),
                        index,
                    },
                    false,
                )
            }),
    };
    match direct {
        Some((method, shy)) => {
            (lookup.include_shy || !shy).then(|| Value::BoundMethod(BoundMethod::new(target.clone(), method)))
        }
        None => {
            let parent = parent_of(class)?;
            let found = lookup_in_class(target, &parent, name, MemberLookup::DEFAULT)?;
            let shy = matches!(&found, Value::BoundMethod(m) if matches!(m.method(), MethodRef::Builtin { method, .. } if method.shy()));
            (lookup.include_shy || !shy).then_some(found)
        }
    }
}

fn class_member_names(class: &ClassRef, include_private: bool) -> Vec<String> {
    match class {
        ClassRef::Builtin(c) => {
            let mut names: Vec<String> = c.fields.iter().map(|f| f.name.to_string()).collect();
            for m in c.methods.iter().filter(|m| include_private || !m.private()) {
                names.push(m.name().to_string());
                names.extend(m.aliases().iter().map(|a| a.to_string()));
            }
            names
        }
        ClassRef::User(c) => {
            let mut names: Vec<String> = c.params().params.iter().map(|p| p.name().to_string()).collect();
            for m in c.methods().iter().filter(|m| include_private || !m.private) {
                names.push(m.name().to_string());
                names.extend(m.aliases.iter().map(|a| a.to_string()));
            }
            names
        }
    }
}
