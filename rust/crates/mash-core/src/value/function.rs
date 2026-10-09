use std::fmt;
use std::rc::Rc;

use super::gc::{Gc, Trace, Tracer};
use super::{BuiltinClass, Class, Scope, Value};
use crate::ast::FnDef;
use crate::function::{BuiltinFunction, BuiltinMethod, Params, Suspended};

/// A function value that isn't a builtin: a user-defined function or lambda, or one of a few function
/// values that carry data.
#[derive(Clone)]
pub struct Function(Gc<FunctionData>);

struct FunctionData {
    kind: FunctionKind,
}

#[derive(Debug)]
pub enum FunctionKind {
    /// `def f x = …` or `x => …`, closing over the scope it was created in. `params` is computed (attributes
    /// evaluated) when the definition is evaluated.
    Closure {
        def: Rc<FnDef>,
        params: Rc<Params>,
        scope: Scope,
    },
    /// `!ls`: runs a system command
    SystemCommand(Rc<str>),
    /// `Point.new` for a user-defined class
    Constructor(Class),
    /// An argument to a `@lazy` parameter: evaluated each time it's called
    Suspended(Suspended),
}

// SAFETY: visits every `Gc` (in scopes, classes and suspended values); `def` and `params` are plain data (the
// AST invariant: no Values).
#[allow(unsafe_code)]
unsafe impl Trace for FunctionData {
    fn trace(&self, tracer: &mut Tracer) {
        match &self.kind {
            FunctionKind::Closure { scope, .. } => scope.trace(tracer),
            FunctionKind::Suspended(suspended) => suspended.trace(tracer),
            FunctionKind::Constructor(class) => class.trace(tracer),
            FunctionKind::SystemCommand(_) => {}
        }
    }
}

// SAFETY: visits its one handle.
#[allow(unsafe_code)]
unsafe impl Trace for Function {
    fn trace(&self, tracer: &mut Tracer) {
        tracer.edge(&self.0);
    }
}

impl Function {
    pub fn new(kind: FunctionKind) -> Self {
        Function(Gc::new(FunctionData { kind }))
    }

    pub fn closure(def: Rc<FnDef>, params: Rc<Params>, scope: Scope) -> Self {
        Function::new(FunctionKind::Closure { def, params, scope })
    }

    pub fn kind(&self) -> &FunctionKind {
        &self.0.kind
    }

    /// The function's name, if it has one (lambdas don't)
    pub fn name(&self) -> Option<&str> {
        match &self.0.kind {
            FunctionKind::Closure { def, .. } => def.name.as_deref(),
            FunctionKind::Constructor(_) => Some("new"),
            FunctionKind::SystemCommand(_) | FunctionKind::Suspended(_) => None,
        }
    }

    pub fn ptr_eq(&self, other: &Function) -> bool {
        Gc::ptr_eq(&self.0, &other.0)
    }

    pub(crate) fn addr(&self) -> usize {
        self.0.addr()
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0.kind {
            FunctionKind::Closure { def, .. } => match &def.name {
                Some(name) => write!(f, "<function {name}>"),
                None => f.write_str("<anonymous function>"),
            },
            FunctionKind::SystemCommand(command) => write!(f, "<system command {command}>"),
            FunctionKind::Constructor(class) => write!(f, "<constructor {}>", class.full_name()),
            FunctionKind::Suspended(_) => f.write_str("<suspended value>"),
        }
    }
}

/// A method bound to its target, e.g. the value of `'foo'.startsWith` or `point.distance`.
#[derive(Clone)]
pub struct BoundMethod(Gc<BoundData>);

struct BoundData {
    target: Value,
    method: MethodRef,
}

// SAFETY: visits the target and the method's class.
#[allow(unsafe_code)]
unsafe impl Trace for BoundData {
    fn trace(&self, tracer: &mut Tracer) {
        self.target.trace(tracer);
        if let MethodRef::User { class, .. } = &self.method {
            class.trace(tracer);
        }
    }
}

// SAFETY: visits its one handle.
#[allow(unsafe_code)]
unsafe impl Trace for BoundMethod {
    fn trace(&self, tracer: &mut Tracer) {
        tracer.edge(&self.0);
    }
}

#[derive(Clone)]
pub enum MethodRef {
    /// A builtin method, found in the given class
    Builtin {
        method: &'static dyn BuiltinMethod,
        class: &'static BuiltinClass,
    },
    /// The method with this index in a user-defined class
    User { class: Class, index: usize },
}

impl fmt::Debug for MethodRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<method {}.{}>", self.class_name(), self.name())
    }
}

impl MethodRef {
    pub fn name(&self) -> &str {
        match self {
            MethodRef::Builtin { method, .. } => method.name(),
            MethodRef::User { class, index } => class.methods()[*index].name(),
        }
    }

    fn same_as(&self, other: &MethodRef) -> bool {
        match (self, other) {
            (MethodRef::Builtin { method: m1, .. }, MethodRef::Builtin { method: m2, .. }) => {
                std::ptr::addr_eq(*m1 as *const dyn BuiltinMethod, *m2 as *const dyn BuiltinMethod)
            }
            (MethodRef::User { class: c1, index: i1 }, MethodRef::User { class: c2, index: i2 }) => {
                c1.ptr_eq(c2) && i1 == i2
            }
            _ => false,
        }
    }

    fn class_name(&self) -> String {
        match self {
            MethodRef::Builtin { class, .. } => class.full_name(),
            MethodRef::User { class, .. } => class.full_name(),
        }
    }
}

impl BoundMethod {
    pub fn new(target: Value, method: MethodRef) -> Self {
        BoundMethod(Gc::new(BoundData { target, method }))
    }

    pub fn target(&self) -> &Value {
        &self.0.target
    }

    pub fn method(&self) -> &MethodRef {
        &self.0.method
    }

    pub fn name(&self) -> &str {
        self.0.method.name()
    }

    /// e.g. `core.String.startsWith`
    pub fn full_name(&self) -> String {
        format!("{}.{}", self.0.method.class_name(), self.name())
    }

    /// Same method bound to equal (key-equal) targets
    pub(crate) fn same_as(&self, other: &BoundMethod) -> bool {
        self.0.method.same_as(&other.0.method) && self.0.target == other.0.target
    }
}

impl fmt::Debug for BoundMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<bound method {}>", self.full_name())
    }
}

/// A builtin function as a value. Builtins are `'static`, so this is just a reference.
#[derive(Clone, Copy)]
pub struct Builtin(pub &'static dyn BuiltinFunction);

impl Builtin {
    pub fn ptr_eq(self, other: Builtin) -> bool {
        std::ptr::addr_eq(
            self.0 as *const dyn BuiltinFunction,
            other.0 as *const dyn BuiltinFunction,
        )
    }

    pub(crate) fn addr(self) -> usize {
        self.0 as *const dyn BuiltinFunction as *const () as usize
    }
}

impl fmt::Debug for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<function {}>", self.0.full_name())
    }
}
