//! The types of runtime values (Scala `ValueTypeDetector`), used for the types of global variables.

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use super::types::*;
use crate::builtins::collections::GROUP_CLASS;
use crate::builtins::core::*;
use crate::builtins::time::{DATE_CLASS, DATE_TIME_CLASS};
use crate::value::{Class, ClassRef, Function, FunctionKind, MethodRef, Object, Scope, Value};

/// Works out the types of values, caching results by identity (so shared values are visited once, and cyclic
/// values terminate).
#[derive(Default)]
pub struct TypeDetector {
    cache: HashMap<usize, Type>,
    visiting: HashSet<usize>,
    classes: HashMap<usize, Rc<UserClassType>>,
    scopes: HashMap<usize, Bindings>,
}

impl std::fmt::Debug for TypeDetector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TypeDetector({} cached)", self.cache.len())
    }
}

/// A heap value's identity, for caching
fn identity(value: &Value) -> Option<usize> {
    match value {
        Value::List(xs) => Some(xs.addr()),
        Value::Object(o) => Some(o.addr()),
        Value::Function(f) => Some(f.addr()),
        _ => None,
    }
}

impl TypeDetector {
    pub fn type_of(&mut self, value: &Value) -> Type {
        let Some(id) = identity(value) else {
            return self.compute(value);
        };
        if let Some(ty) = self.cache.get(&id) {
            return ty.clone();
        }
        if !self.visiting.insert(id) {
            return Type::ANY;
        }
        let ty = self.compute(value);
        self.visiting.remove(&id);
        self.cache.insert(id, ty.clone());
        ty
    }

    fn compute(&mut self, value: &Value) -> Type {
        match value {
            Value::Null => Type::NULL,
            Value::Unit => Type::UNIT,
            Value::Bool(_) => Type::BOOLEAN,
            Value::Number(_, None) => Type::NUMBER,
            Value::Number(_, Some(tag)) => Type::tagged_number(tag),
            Value::Str(s) => match s.tag() {
                None => Type::STRING,
                Some(tag) => Type::tagged_string(tag),
            },
            Value::DateTime(_) => Type::Instance(&DATE_TIME_CLASS),
            Value::Date(_) => Type::Instance(&DATE_CLASS),
            Value::List(xs) => Type::seq(xs.first().map(|x| self.type_of(&x)).unwrap_or(Type::ANY)),
            Value::Object(o) => self.object_type(o),
            Value::Builtin(b) => Type::BuiltinFunction(BuiltinFunctionType::Builtin(b.0)),
            Value::Function(f) => self.function_type(f),
            Value::BoundMethod(m) => {
                let target = Rc::new(self.type_of(m.target()));
                match m.method() {
                    MethodRef::Builtin { method, .. } => Type::BoundBuiltinMethod(target, *method),
                    MethodRef::User { class, index } => {
                        let class_type = self.user_class(class);
                        let method = &class.methods()[*index];
                        match class_type.method(method.name()) {
                            Some(m) => Type::BoundUserMethod(target, m.clone()),
                            None => Type::ANY,
                        }
                    }
                }
            }
            Value::Class(ClassRef::User(class)) => Type::UserClass(self.user_class(class)),
            Value::Class(ClassRef::Builtin(_)) => Type::CLASS,
        }
    }

    fn object_type(&mut self, object: &Object) -> Type {
        match object.class() {
            None => {
                let fields: Vec<(Rc<str>, Type)> = object
                    .entries()
                    .iter()
                    .filter_map(|(k, v)| Some((Rc::from(k.as_str()?), self.type_of(v))))
                    .collect();
                Type::object(fields)
            }
            Some(ClassRef::User(class)) => Type::UserClassInstance(self.user_class(&class)),
            Some(ClassRef::Builtin(class)) if std::ptr::eq(class, &GROUP_CLASS) => {
                let key = object.get_str("key").map(|k| self.type_of(&k)).unwrap_or(Type::ANY);
                let element = match object.get_str("values").map(|v| self.type_of(&v)) {
                    Some(Type::Seq(element)) => (*element).clone(),
                    _ => Type::ANY,
                };
                Type::generic(&GROUP_CLASS, [key, element])
            }
            Some(ClassRef::Builtin(class)) if std::ptr::eq(class, &TIMED_RESULT_CLASS) => {
                let result = object.get_str("result").map(|r| self.type_of(&r)).unwrap_or(Type::ANY);
                Type::generic(&TIMED_RESULT_CLASS, [result])
            }
            Some(ClassRef::Builtin(class)) => Type::Instance(class),
        }
    }

    fn function_type(&mut self, function: &Function) -> Type {
        match function.kind() {
            FunctionKind::Closure { def, params, scope } => Type::UserFunction(Rc::new(UserFunctionType {
                name: def.name.clone(),
                doc: def.doc.clone(),
                private: false,
                params: params.clone(),
                def: def.clone(),
                bindings: FunctionBindings::Scope(scope.clone(), Rc::default()),
            })),
            FunctionKind::Constructor(class) => {
                Type::BuiltinFunction(BuiltinFunctionType::Constructor(self.user_class(class)))
            }
            FunctionKind::SystemCommand(_) => Type::BuiltinFunction(BuiltinFunctionType::SystemCommand),
            FunctionKind::Suspended(_) => Type::FUNCTION,
        }
    }

    /// The type of a user-defined class (one per class, so types compare equal by identity)
    pub fn user_class(&mut self, class: &Class) -> Rc<UserClassType> {
        if let Some(existing) = self.classes.get(&class.addr()) {
            return existing.clone();
        }
        let mut methods = vec![];
        let mut method_bindings: Vec<(Rc<str>, Type)> = vec![];
        for method in class.methods() {
            let bindings = FunctionBindings::Scope(
                class.scope().clone(),
                Rc::new(method_bindings.iter().cloned().collect()),
            );
            let function = Rc::new(UserFunctionType {
                name: method.def.name.clone(),
                doc: method.def.doc.clone(),
                private: method.private,
                params: method.params.clone(),
                def: method.def.clone(),
                bindings,
            });
            let names = std::iter::once(Rc::from(method.name())).chain(method.aliases.iter().cloned());
            for name in names {
                methods.push((name.clone(), function.clone()));
                method_bindings.push((name, Type::UserFunction(function.clone())));
            }
        }
        let class_type = Rc::new(UserClassType {
            name: class.name().into(),
            params: class.params().clone(),
            def: Some(class.def().clone()),
            methods,
        });
        self.classes.insert(class.addr(), class_type.clone());
        class_type
    }

    /// The types of the variables visible in a scope (innermost wins), cached per scope
    pub fn scope_bindings(&mut self, scope: &Scope) -> Bindings {
        let id = scope_id(scope);
        if let Some(bindings) = self.scopes.get(&id) {
            return bindings.clone();
        }
        let outer = match scope.parent() {
            Some(parent) => self.scope_bindings(parent),
            None => Rc::default(),
        };
        let mut own = vec![];
        for (k, v) in scope.vars().entries() {
            if let Some(name) = k.as_str() {
                own.push((Rc::from(name), self.type_of(&v)));
            }
        }
        let bindings = extend_bindings(&outer, own);
        self.scopes.insert(id, bindings.clone());
        bindings
    }
}

fn scope_id(scope: &Scope) -> usize {
    scope.vars().addr()
}
