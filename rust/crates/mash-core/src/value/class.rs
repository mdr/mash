use std::fmt;
use std::rc::Rc;

use dumpster::{Trace, TraceWith, Visitor, unsync::Gc};

use super::gc::Tracked;
use super::{Scope, Tag};
use crate::ast::{ClassDef, FnDef};
use crate::function::{BuiltinFunction, BuiltinMethod, Params};

/// A class defined in Rust. Static data, never collected.
pub struct BuiltinClass {
    /// e.g. `Path`
    pub name: &'static str,
    /// e.g. `os`
    pub namespace: &'static str,
    /// None only for `core.Any`
    pub parent: Option<&'static BuiltinClass>,
    pub summary: &'static str,
    pub description: Option<&'static str>,
    pub fields: &'static [Field],
    pub methods: &'static [&'static dyn BuiltinMethod],
    pub static_methods: &'static [&'static dyn BuiltinFunction],
}

/// A field that instances of a builtin class have.
#[derive(Debug)]
pub struct Field {
    pub name: &'static str,
    pub summary: &'static str,
}

impl BuiltinClass {
    /// e.g. `os.Path`
    pub fn full_name(&self) -> String {
        if self.namespace.is_empty() {
            self.name.to_string()
        } else {
            format!("{}.{}", self.namespace, self.name)
        }
    }

    /// An empty class (fill in with struct update syntax)
    pub const fn new(namespace: &'static str, name: &'static str, parent: Option<&'static BuiltinClass>) -> Self {
        BuiltinClass {
            name,
            namespace,
            parent,
            summary: "",
            description: None,
            fields: &[],
            methods: &[],
            static_methods: &[],
        }
    }

    /// A method of this class (not its parents), by name or alias
    pub fn method(&self, name: &str) -> Option<&'static dyn BuiltinMethod> {
        self.methods
            .iter()
            .copied()
            .find(|m| m.name() == name || m.aliases().contains(&name))
    }

    pub fn static_method(&self, name: &str) -> Option<&'static dyn BuiltinFunction> {
        self.static_methods.iter().copied().find(|m| m.name() == name)
    }

    pub fn is_subclass_of(&'static self, other: &'static BuiltinClass) -> bool {
        let mut class = Some(self);
        while let Some(c) = class {
            if std::ptr::eq(c, other) {
                return true;
            }
            class = c.parent;
        }
        false
    }
}

impl fmt::Debug for BuiltinClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<class {}>", self.full_name())
    }
}

/// A user-defined class (`class Point x y { def … }`).
#[derive(Clone, Trace)]
pub struct Class(Gc<ClassData>);

struct ClassData {
    def: Rc<ClassDef>,
    /// Constructor parameters (instance fields)
    params: Rc<Params>,
    methods: Vec<UserMethod>,
    /// The scope the class was declared in, which method bodies close over
    scope: Scope,
    _tracked: Tracked,
}

// SAFETY: the only `Gc` is in `scope`; `def`, `params` and `methods` are plain data (the AST invariant: no
// Values).
#[allow(unsafe_code)]
unsafe impl<V: Visitor> TraceWith<V> for ClassData {
    fn accept(&self, visitor: &mut V) -> Result<(), ()> {
        self.scope.accept(visitor)
    }
}

/// A method of a user-defined class, with its attributes evaluated.
#[derive(Debug)]
pub struct UserMethod {
    pub def: Rc<FnDef>,
    pub params: Rc<Params>,
    pub aliases: Vec<Rc<str>>,
    pub private: bool,
}

impl UserMethod {
    pub fn name(&self) -> &str {
        self.def.name.as_deref().unwrap_or_default()
    }

    pub fn has_name(&self, name: &str) -> bool {
        self.name() == name || self.aliases.iter().any(|a| &**a == name)
    }
}

impl Class {
    pub fn new(def: Rc<ClassDef>, params: Rc<Params>, methods: Vec<UserMethod>, scope: Scope) -> Self {
        Class(Gc::new(ClassData {
            def,
            params,
            methods,
            scope,
            _tracked: Tracked::new(),
        }))
    }

    pub fn name(&self) -> &str {
        &self.0.def.name
    }

    pub fn full_name(&self) -> String {
        match &self.0.def.namespace {
            Some(ns) if !ns.is_empty() => format!("{}.{}", ns.join("."), self.name()),
            _ => self.name().to_string(),
        }
    }

    pub fn def(&self) -> &Rc<ClassDef> {
        &self.0.def
    }

    pub fn params(&self) -> &Rc<Params> {
        &self.0.params
    }

    pub fn methods(&self) -> &[UserMethod] {
        &self.0.methods
    }

    /// Index of the method with this name or alias
    pub fn find_method(&self, name: &str) -> Option<usize> {
        self.0.methods.iter().position(|m| m.has_name(name))
    }

    pub fn scope(&self) -> &Scope {
        &self.0.scope
    }

    pub fn ptr_eq(&self, other: &Class) -> bool {
        Gc::ptr_eq(&self.0, &other.0)
    }

    pub(crate) fn addr(&self) -> usize {
        Gc::as_ptr(&self.0) as *const () as usize
    }
}

impl fmt::Debug for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<class {}>", self.full_name())
    }
}

/// A class: builtins are `'static`, user classes are collected values.
#[derive(Clone, Trace)]
pub enum ClassRef {
    Builtin(&'static BuiltinClass),
    User(Class),
}

impl ClassRef {
    pub fn name(&self) -> String {
        match self {
            ClassRef::Builtin(c) => c.name.to_string(),
            ClassRef::User(c) => c.name().to_string(),
        }
    }

    pub fn full_name(&self) -> String {
        match self {
            ClassRef::Builtin(c) => c.full_name(),
            ClassRef::User(c) => c.full_name(),
        }
    }

    pub fn as_builtin(&self) -> Option<Tag> {
        match self {
            ClassRef::Builtin(c) => Some(c),
            ClassRef::User(_) => None,
        }
    }

    pub fn ptr_eq(&self, other: &ClassRef) -> bool {
        match (self, other) {
            (ClassRef::Builtin(a), ClassRef::Builtin(b)) => std::ptr::eq(*a, *b),
            (ClassRef::User(a), ClassRef::User(b)) => a.ptr_eq(b),
            _ => false,
        }
    }

    pub fn is_builtin(&self, class: &'static BuiltinClass) -> bool {
        matches!(self, ClassRef::Builtin(c) if std::ptr::eq(*c, class))
    }

    pub(crate) fn addr(&self) -> usize {
        match self {
            ClassRef::Builtin(c) => *c as *const BuiltinClass as usize,
            ClassRef::User(c) => c.addr(),
        }
    }
}

impl fmt::Debug for ClassRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ClassRef::Builtin(c) => fmt::Debug::fmt(c, f),
            ClassRef::User(c) => fmt::Debug::fmt(c, f),
        }
    }
}
