//! The tree-walking evaluator (Scala `Evaluator` and friends).
//!
//! * `ctx: &mut Ctx` holds the interpreter's state; the current scope is passed separately (closures capture
//!   it, and it is a cheap handle).
//! * Every fallible operation returns [`R<T>`]: errors carry a Mash-level stack trace, built as they propagate
//!   through calls. There are no panics on user input.
//! * Mash calls recurse on the Rust stack, so the interpreter should run on a thread with a large stack (see
//!   [`with_big_stack`]); a depth limit turns runaway recursion into an error.

mod assign;
mod declare;
mod error;
mod invoke;
mod member;
mod ops;
mod render;

use std::collections::HashSet;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use mash_syntax::{SyntaxError, TextRange};

use crate::ast::*;
use crate::builtins;
use crate::compile::{self, Compiled, HOLE_VARIABLE_PREFIX, Settings};
use crate::platform::Platform;
use crate::value::{List, Object, Scope, ScopeKind, Str, Value};

pub use error::{ErrorKind, Location, MashError, R, StackFrame, suggestion_suffix};
pub use member::{MemberLookup, is_a, is_subclass_of, parent_of, primary_class};
pub use ops::binary_op;
pub use render::render;

/// Default limit on nested Mash function calls.
#[cfg(not(target_arch = "wasm32"))]
pub const DEFAULT_MAX_DEPTH: usize = 10_000;
/// Default limit on nested Mash function calls (the browser has a smaller, fixed stack).
#[cfg(target_arch = "wasm32")]
pub const DEFAULT_MAX_DEPTH: usize = 4_000;

/// The interpreter: global state, the platform, and evaluation.
pub struct Ctx {
    global_scope: Scope,
    platform: Rc<dyn Platform>,
    ids: NodeIdGen,
    /// The compilation unit of the code being evaluated (for error locations)
    unit: Rc<SourceUnit>,
    /// Mash calls in progress
    depth: usize,
    pub max_depth: usize,
    interrupt: Arc<AtomicBool>,
    /// If set, evaluation stops after this many steps (for the browser, which has no Ctrl-C)
    pub step_budget: Option<u64>,
    steps: u64,
    pub settings: Settings,
}

impl std::fmt::Debug for Ctx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Ctx")
            .field("platform", &self.platform)
            .field("depth", &self.depth)
            .finish_non_exhaustive()
    }
}

/// Why running some source failed.
#[derive(Debug)]
pub enum RunError {
    Syntax(Vec<SyntaxError>),
    Eval(MashError),
}

impl std::fmt::Display for RunError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RunError::Syntax(errors) => {
                let messages: Vec<&str> = errors.iter().map(|e| e.message.as_str()).collect();
                write!(f, "{}", messages.join("; "))
            }
            RunError::Eval(e) => write!(f, "{e}"),
        }
    }
}

impl Ctx {
    /// An interpreter with the standard global environment.
    pub fn new(platform: Rc<dyn Platform>) -> Ctx {
        let globals = builtins::standard_globals(platform.as_ref());
        Ctx {
            global_scope: Scope::global(globals),
            platform,
            ids: NodeIdGen::default(),
            unit: SourceUnit::new("<none>", ""),
            depth: 0,
            max_depth: DEFAULT_MAX_DEPTH,
            interrupt: Arc::new(AtomicBool::new(false)),
            step_budget: None,
            steps: 0,
            settings: Settings::default(),
        }
    }

    pub fn globals(&self) -> &Object {
        self.global_scope.vars()
    }

    pub fn global_scope(&self) -> &Scope {
        &self.global_scope
    }

    pub fn platform(&self) -> &dyn Platform {
        self.platform.as_ref()
    }

    /// Setting this flag (e.g. from a Ctrl-C handler) interrupts evaluation
    pub fn interrupt_flag(&self) -> Arc<AtomicBool> {
        self.interrupt.clone()
    }

    pub fn time_zone(&self) -> jiff::tz::TimeZone {
        self.platform.time_zone()
    }

    /// Compile source text, with the current global names as bindings (for bare words)
    pub fn compile(&mut self, unit: Rc<SourceUnit>) -> Result<Compiled, Vec<SyntaxError>> {
        let bindings = self.global_names();
        compile::compile(unit, self.settings, &bindings, &mut self.ids)
    }

    fn global_names(&self) -> HashSet<Rc<str>> {
        self.globals()
            .keys()
            .iter()
            .filter_map(|k| k.as_str().map(Rc::from))
            .collect()
    }

    /// Compile and evaluate source text in the global scope.
    pub fn run(&mut self, name: &str, text: &str) -> Result<Value, RunError> {
        let compiled = self.compile(SourceUnit::new(name, text)).map_err(RunError::Syntax)?;
        self.eval_program(&compiled.program).map_err(RunError::Eval)
    }

    /// Evaluate a compiled program in the global scope.
    pub fn eval_program(&mut self, program: &Program) -> R<Value> {
        self.interrupt.store(false, Ordering::Relaxed);
        self.steps = 0;
        let scope = self.global_scope.clone();
        self.with_unit(program.unit.clone(), |ctx| ctx.eval(&program.body, &scope))
    }

    /// Run `f` with `unit` as the current compilation unit
    pub(crate) fn with_unit<T>(&mut self, unit: Rc<SourceUnit>, f: impl FnOnce(&mut Ctx) -> T) -> T {
        let previous = std::mem::replace(&mut self.unit, unit);
        let result = f(self);
        self.unit = previous;
        result
    }

    /// A location in the current compilation unit
    pub(crate) fn loc(&self, range: Option<TextRange>) -> Option<Location> {
        range.map(|range| Location {
            unit: self.unit.clone(),
            range,
        })
    }

    fn check_interrupted(&mut self) -> R<()> {
        if self.interrupt.load(Ordering::Relaxed) {
            return Err(MashError::interrupted());
        }
        if let Some(budget) = self.step_budget {
            self.steps += 1;
            if self.steps > budget {
                return Err(MashError::interrupted());
            }
        }
        Ok(())
    }

    // ------------------------------------------------------------------------------------------- evaluation

    /// Evaluate an expression. A name or member that refers to a function that can be called with no
    /// arguments is called (Scala `Evaluator.evaluate`).
    pub fn eval(&mut self, e: &E, scope: &Scope) -> R<Value> {
        self.check_interrupted()?;
        match &e.kind {
            ExprKind::Identifier(name) => {
                let binding = self.lookup_identifier(name, e.range, scope)?;
                if binding.safe {
                    Ok(binding.value)
                } else {
                    self.invoke_nullary(binding.value, self.loc(e.range))
                }
            }
            ExprKind::Member { .. } => {
                let value = self.simple_eval(e, scope)?;
                self.invoke_nullary(value, self.loc(e.range))
            }
            _ => self.simple_eval(e, scope),
        }
    }

    /// Evaluate an expression without calling the resulting function, if it is one (Scala
    /// `Evaluator.simpleEvaluate`).
    pub fn simple_eval(&mut self, e: &E, scope: &Scope) -> R<Value> {
        use ExprKind as K;
        match &e.kind {
            K::Literal(Literal::Number(n)) => Ok(Value::num(*n)),
            K::Literal(Literal::Bool(b)) => Ok(Value::Bool(*b)),
            K::Literal(Literal::Null) => Ok(Value::Null),
            K::String { value, quote, tilde } => Ok(self.string_literal(value, *quote == QuoteKind::Double, *tilde)),
            K::Interpolated { parts, tilde } => self.interpolated_string(parts, *tilde, scope),
            K::Identifier(name) => Ok(self.lookup_identifier(name, e.range, scope)?.value),
            K::This => scope
                .this()
                .ok_or_else(|| MashError::at("No binding for 'this'", self.loc(e.range))),
            K::Paren(inner) => self.eval(inner, scope),
            K::Block(body) => {
                let block_scope = scope.child(ScopeKind::Leaky, Object::new(), vec![]);
                self.eval(body, &block_scope)
            }
            K::StatementSeq(statements) => {
                let mut result = Value::Unit;
                for statement in statements {
                    result = self.eval(statement, scope)?;
                }
                Ok(result)
            }
            K::List(items) => {
                let mut values = Vec::with_capacity(items.len());
                for item in items {
                    values.push(self.eval(item, scope)?);
                }
                Ok(Value::List(List::new(values)))
            }
            K::Object(entries) => self.object_literal(entries, scope),
            K::Member {
                target,
                name,
                null_safe,
                name_range,
            } => {
                let this_target = matches!(target.kind, K::This);
                let target = self.eval(target, scope)?;
                let location = self.loc(name_range.or(e.range));
                Ok(self
                    .member(&target, name, *null_safe, true, this_target, location)?
                    .value)
            }
            K::Lookup { target, index } => self.lookup_expr(e, target, index, scope),
            K::Invocation { function, args, .. } => self.invocation(e, function, args, scope),
            K::Minus(inner) => match self.eval(inner, scope)? {
                Value::Number(n, tag) => Ok(Value::Number((-n.as_f64()).into(), tag)),
                other => Err(MashError::at(
                    format!("Could not negate a value of type {}", other.type_name()),
                    self.loc(inner.range),
                )),
            },
            K::BinOp {
                op: BinOp::And,
                lhs,
                rhs,
            } => {
                let left = self.eval(lhs, scope)?;
                if left.is_truthy() {
                    self.eval(rhs, scope)
                } else {
                    Ok(left)
                }
            }
            K::BinOp {
                op: BinOp::Or,
                lhs,
                rhs,
            } => {
                let left = self.eval(lhs, scope)?;
                if left.is_truthy() {
                    Ok(left)
                } else {
                    self.eval(rhs, scope)
                }
            }
            K::BinOp { op, lhs, rhs } => {
                let left = self.eval(lhs, scope)?;
                let right = self.eval(rhs, scope)?;
                self.binary_op(*op, &left, &right, e.range)
            }
            K::ChainedOp { first, rest } => {
                let mut previous = self.eval(first, scope)?;
                let mut success = true;
                for (op, rhs) in rest {
                    let value = self.eval(rhs, scope)?;
                    let this_success = self.binary_op(*op, &previous, &value, e.range)?.is_truthy();
                    if success {
                        previous = value;
                    }
                    success &= this_success;
                }
                Ok(Value::Bool(success))
            }
            K::If { cond, then, else_ } => {
                if self.eval(cond, scope)?.is_truthy() {
                    self.eval(then, scope)
                } else {
                    match else_ {
                        Some(else_) => self.eval(else_, scope),
                        None => Ok(Value::Unit),
                    }
                }
            }
            K::Assignment { lhs, op, rhs } => self.assignment(e, lhs, *op, rhs, scope),
            K::PatternAssignment { pattern, rhs } => {
                let value = self.eval(rhs, scope)?;
                self.bind_pattern(pattern, &value, &mut |name, v| scope.assign(name, v), self.loc(e.range))?;
                Ok(value)
            }
            K::Lambda(def) => self.lambda(def, scope),
            K::FunctionDecl(def) => self.function_decl(def, scope),
            K::ClassDecl(def) => self.class_decl(def, scope),
            K::MishFunction(command) => Ok(Value::Function(crate::value::Function::new(
                crate::value::FunctionKind::SystemCommand(command.clone()),
            ))),
            K::Mish { .. } | K::MishInterpolation(_) => Err(MashError::at(
                "Running system commands is not supported yet",
                self.loc(e.range),
            )),
            K::Help(inner) => self.help(inner, scope),
            K::Import { expr, name } => self.import(e, expr, name.as_deref(), scope),
            K::Missing => Err(MashError::at("Incomplete expression", self.loc(e.range))),
            K::Hole(_) | K::HeadlessMember { .. } | K::Pipe { .. } => Err(MashError::at(
                "Internal error: unexpected syntax after desugaring",
                self.loc(e.range),
            )),
        }
    }

    fn lookup_identifier(&self, name: &str, range: Option<TextRange>, scope: &Scope) -> R<crate::value::Binding> {
        let binding = scope.lookup_with(name, |this, name| {
            self.lookup_member(
                this,
                name,
                MemberLookup {
                    include_private: true,
                    include_shy: false,
                },
            )
        });
        binding.ok_or_else(|| {
            let names = scope.all_names();
            MashError::at(
                format!("No binding for '{name}'{}", suggestion_suffix(&names, name)),
                self.loc(range),
            )
        })
    }

    fn string_literal(&self, value: &str, double_quoted: bool, tilde: bool) -> Value {
        let text: Rc<str> = if tilde {
            format!("{}{value}", self.home_dir()).into()
        } else {
            value.into()
        };
        if double_quoted {
            Value::Str(Str::tagged(text, &builtins::os::PATH_CLASS))
        } else {
            Value::Str(Str::new(text))
        }
    }

    fn home_dir(&self) -> String {
        self.platform.home_dir().unwrap_or_default()
    }

    /// Text parts are path-tagged, so the result is a path (as in Scala, where the result keeps the tag of the
    /// first part)
    fn interpolated_string(&mut self, parts: &[InterpolationPart], tilde: bool, scope: &Scope) -> R<Value> {
        let mut text = if tilde { self.home_dir() } else { String::new() };
        for part in parts {
            match part {
                InterpolationPart::Text(t) => text.push_str(t),
                InterpolationPart::Expr(e) => {
                    let value = self.eval(e, scope)?;
                    match &value {
                        Value::Str(s) => text.push_str(s.as_str()),
                        _ => text.push_str(&self.stringify(&value)?),
                    }
                }
            }
        }
        Ok(Value::Str(Str::tagged(text, &builtins::os::PATH_CLASS)))
    }

    fn object_literal(&mut self, entries: &[ObjectEntry], scope: &Scope) -> R<Value> {
        let object = Object::new();
        for entry in entries {
            match entry {
                ObjectEntry::Full { field, value } => {
                    let key = match &field.kind {
                        ExprKind::Identifier(name) if !name.starts_with(HOLE_VARIABLE_PREFIX) => Value::str(name),
                        _ => self.eval(field, scope)?,
                    };
                    let value = self.eval(value, scope)?;
                    object.set(key, value);
                }
                ObjectEntry::Shorthand { name, range } => {
                    let value = self.lookup_identifier(name, *range, scope)?.value;
                    let value = self.invoke_nullary(value, self.loc(*range))?;
                    object.set_str(name, value);
                }
            }
        }
        Ok(Value::Object(object))
    }

    fn lookup_expr(&mut self, e: &E, target: &E, index: &E, scope: &Scope) -> R<Value> {
        let this_target = matches!(target.kind, ExprKind::This);
        let target = self.eval(target, scope)?;
        let index_value = self.eval(index, scope)?;
        let lookup = MemberLookup {
            include_private: this_target,
            include_shy: true,
        };
        let not_found =
            |ctx: &Ctx, target: &Value, name: &str| ctx.member_not_found(target, name, ctx.loc(index.range));
        match (&index_value, &target) {
            (Value::Str(name), _) => self
                .lookup_member(&target, name.as_str(), lookup)
                .ok_or_else(|| not_found(self, &target, name.as_str())),
            (Value::Number(n, _), _) => {
                let Some(i) = n.as_i64() else {
                    return Err(MashError::at(
                        format!("Unable to lookup, non-integer index: {n}"),
                        self.loc(e.range),
                    ));
                };
                match &target {
                    Value::List(xs) => xs
                        .resolve_index(i)
                        .and_then(|i| xs.get(i as i64))
                        .ok_or_else(|| MashError::at(format!("Index out of range {n}"), self.loc(index.range))),
                    Value::Str(s) => match s.char_at(i) {
                        Some(c) => Ok(Value::Str(Str::tagged(c.to_string(), &builtins::core::CHARACTER_CLASS))),
                        None => Err(MashError::at(format!("Index out of range {n}"), self.loc(index.range))),
                    },
                    Value::Object(o) => o.get(&index_value).ok_or_else(|| {
                        MashError::at(
                            format!("Cannot find member '{n}' in value of type {}", target.type_name()),
                            self.loc(index.range),
                        )
                    }),
                    _ => Err(MashError::at(
                        format!("Unable to lookup in target of type {}", target.type_name()),
                        self.loc(e.range),
                    )),
                }
            }
            (_, Value::Object(o)) => o.get(&index_value).ok_or_else(|| {
                let name = render(&index_value);
                MashError::at(
                    format!("Cannot find member '{name}' in value of type {}", target.type_name()),
                    self.loc(index.range),
                )
            }),
            _ => Err(MashError::at(
                format!("Unable to lookup index of type {}", index_value.type_name()),
                self.loc(index.range),
            )),
        }
    }

    fn import(&mut self, e: &E, expr: &E, name: Option<&str>, scope: &Scope) -> R<Value> {
        let target = self.eval(expr, scope)?;
        match name {
            Some(name) => {
                let lookup = MemberLookup {
                    include_private: false,
                    include_shy: true,
                };
                match self.lookup_member(&target, name, lookup) {
                    Some(value) => {
                        scope.assign(name, value.clone());
                        Ok(value)
                    }
                    None => Err(self.member_not_found(&target, name, self.loc(e.range))),
                }
            }
            None => {
                let lookup = MemberLookup {
                    include_private: false,
                    include_shy: false,
                };
                for name in self.member_names(&target, false) {
                    if let Some(value) = self.lookup_member(&target, &name, lookup) {
                        scope.assign(&name, value);
                    }
                }
                Ok(Value::Unit)
            }
        }
    }

    /// `expr?` (Scala `HelpEvaluator`): for `target.name`, help for the field or method if there is one (or for
    /// that member of a list's first element), else help for the member's value
    fn help(&mut self, inner: &E, scope: &Scope) -> R<Value> {
        use builtins::core::help::{help_for, help_for_member};
        let value = match &inner.kind {
            ExprKind::Member { target, name, .. } => {
                let target = self.eval(target, scope)?;
                if let Some(help) = help_for_member(self, &target, name) {
                    return Ok(help);
                }
                if let Value::List(xs) = &target
                    && let Some(first) = xs.first()
                    && let Some(help) = help_for_member(self, &first, name)
                {
                    return Ok(help);
                }
                self.member(&target, name, false, true, false, self.loc(inner.range))?
                    .value
            }
            _ => self.simple_eval(inner, scope)?,
        };
        help_for(self, &value)
    }

    /// Convert to a string as Mash does for interpolation and `+` (Scala `ToStringifier.stringify`): strings
    /// as they are; anything else via its `toString` method (which a user class may define).
    pub fn stringify(&mut self, value: &Value) -> R<String> {
        if let Value::Str(s) = value {
            return Ok(s.as_str().to_string());
        }
        let lookup = MemberLookup {
            include_private: false,
            include_shy: true,
        };
        let method = self
            .lookup_member(value, "toString", lookup)
            .ok_or_else(|| MashError::new("No toString method"))?;
        match self.invoke_nullary(method, None)? {
            Value::Str(s) => Ok(s.as_str().to_string()),
            other => Err(MashError::new(format!(
                "Invalid toString value of type {}",
                other.type_name()
            ))),
        }
    }
}

/// Run `f` on a thread with a large stack (Mash calls recurse on the Rust stack). The interpreter's values
/// can't cross threads, so the whole interpreter must live inside `f`. On wasm, just runs `f`.
#[cfg(not(target_arch = "wasm32"))]
pub fn with_big_stack<T: Send + 'static>(f: impl FnOnce() -> T + Send + 'static) -> T {
    /// Address space reserved for the stack; pages are only committed as they are used
    const STACK_SIZE: usize = 1 << 30;
    std::thread::Builder::new()
        .name("mash".into())
        .stack_size(STACK_SIZE)
        .spawn(f)
        .expect("failed to start the interpreter thread")
        .join()
        .unwrap_or_else(|panic| std::panic::resume_unwind(panic))
}

#[cfg(target_arch = "wasm32")]
pub fn with_big_stack<T>(f: impl FnOnce() -> T) -> T {
    f()
}
