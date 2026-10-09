use std::fmt;
use std::rc::Rc;

use super::gc::{Gc, Trace, Tracer};
use super::{Object, Value};

/// Whether assignments to unknown names escape a scope.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ScopeKind {
    /// Lambdas and `{ blocks }`: assigning a name not bound here assigns in the enclosing scope
    Leaky,
    /// `def` bodies, methods and compilation units: assignments stay here
    Full,
}

/// A scope: variables (stored in an object, as in Mash), the names among them that are "safe" (not
/// automatically invoked; from `@safe` parameters), an optional `this`, and the enclosing scope.
#[derive(Clone)]
pub struct Scope(Gc<ScopeData>);

struct ScopeData {
    vars: Object,
    safe_names: Vec<Rc<str>>,
    this: Option<Value>,
    kind: ScopeKind,
    parent: Option<Scope>,
}

// SAFETY: visits the variables, `this` and the parent; the rest is plain data.
#[allow(unsafe_code)]
unsafe impl Trace for ScopeData {
    fn trace(&self, tracer: &mut Tracer) {
        self.vars.trace(tracer);
        self.this.trace(tracer);
        self.parent.trace(tracer);
    }
}

// SAFETY: visits its one handle.
#[allow(unsafe_code)]
unsafe impl Trace for Scope {
    fn trace(&self, tracer: &mut Tracer) {
        tracer.edge(&self.0);
    }
}

/// The result of looking up a name.
#[derive(Debug)]
pub struct Binding {
    pub value: Value,
    /// Bound to a `@safe` parameter: nullary functions in it are not invoked automatically
    pub safe: bool,
}

impl Scope {
    /// The outermost scope, whose variables are the global object
    pub fn global(vars: Object) -> Self {
        Scope::make(vars, vec![], None, ScopeKind::Full, None)
    }

    pub fn child(&self, kind: ScopeKind, vars: Object, safe_names: Vec<Rc<str>>) -> Self {
        Scope::make(vars, safe_names, None, kind, Some(self.clone()))
    }

    /// A full scope for a method body, with `this` bound
    pub fn child_with_this(&self, vars: Object, safe_names: Vec<Rc<str>>, this: Value) -> Self {
        Scope::make(vars, safe_names, Some(this), ScopeKind::Full, Some(self.clone()))
    }

    fn make(
        vars: Object,
        safe_names: Vec<Rc<str>>,
        this: Option<Value>,
        kind: ScopeKind,
        parent: Option<Scope>,
    ) -> Self {
        Scope(Gc::new(ScopeData {
            vars,
            safe_names,
            this,
            kind,
            parent,
        }))
    }

    pub fn vars(&self) -> &Object {
        &self.0.vars
    }

    pub fn kind(&self) -> ScopeKind {
        self.0.kind
    }

    pub fn parent(&self) -> Option<&Scope> {
        self.0.parent.as_ref()
    }

    /// `this` in the innermost scope that has one
    pub fn this(&self) -> Option<Value> {
        self.ancestors().find_map(|s| s.0.this.clone())
    }

    /// This scope and its enclosing scopes, innermost first
    pub fn ancestors(&self) -> impl Iterator<Item = &Scope> {
        std::iter::successors(Some(self), |s| s.parent())
    }

    /// Look a name up through the chain: in each scope, its variables, then (via `this_member`) a member of
    /// its `this`.
    pub fn lookup_with(
        &self,
        name: &str,
        mut this_member: impl FnMut(&Value, &str) -> Option<Value>,
    ) -> Option<Binding> {
        for scope in self.ancestors() {
            if let Some(value) = scope.0.vars.get_str(name) {
                let safe = scope.0.safe_names.iter().any(|n| &**n == name);
                return Some(Binding { value, safe });
            }
            if let Some(value) = scope.0.this.as_ref().and_then(|t| this_member(t, name)) {
                return Some(Binding { value, safe: false });
            }
        }
        None
    }

    /// Variable lookup, ignoring members of `this`
    pub fn lookup(&self, name: &str) -> Option<Value> {
        self.lookup_with(name, |_, _| None).map(|b| b.value)
    }

    /// Mash assignment (Scala `ScopeStack.set`): an existing variable in the innermost scope wins, then a
    /// field of that scope's `this` object; otherwise a leaky scope defers to its parent and a full scope
    /// binds the name locally.
    pub fn assign(&self, name: &str, value: Value) {
        let mut scope = self;
        loop {
            if scope.0.vars.contains_str(name) {
                return scope.0.vars.set_str(name, value);
            }
            if let Some(Value::Object(this)) = &scope.0.this
                && this.contains_str(name)
            {
                return this.set_str(name, value);
            }
            match (scope.0.kind, &scope.0.parent) {
                (ScopeKind::Leaky, Some(parent)) => scope = parent,
                _ => return scope.0.vars.set_str(name, value),
            }
        }
    }

    /// Every visible variable name (for "did you mean" suggestions)
    pub fn all_names(&self) -> Vec<String> {
        let mut names = vec![];
        for scope in self.ancestors() {
            for key in scope.0.vars.keys() {
                if let Value::Str(s) = key
                    && !names.iter().any(|n: &String| n == s.as_str())
                {
                    names.push(s.as_str().to_string());
                }
            }
        }
        names
    }

    pub fn ptr_eq(&self, other: &Scope) -> bool {
        Gc::ptr_eq(&self.0, &other.0)
    }
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<scope ({:?}, depth {})>", self.0.kind, self.ancestors().count())
    }
}
