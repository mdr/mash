//! Calling things (Scala `InvocationEvaluator`, and the `call` methods of each kind of function).

use std::rc::Rc;
use std::sync::LazyLock;

use crate::ast::{Arg, E, ExprKind, FnDef};
use crate::function::{Args, Argument, BuiltinFunction, BuiltinMethod, Param, Params, Suspended, bind};
use crate::value::{BoundMethod, Class, ClassRef, Function, FunctionKind, MethodRef, Object, Scope, ScopeKind, Value};

use super::{Ctx, ErrorKind, Location, MashError, R};

/// Parameters of a string used as a function: `'name' x` means `x.name`
static STRING_FUNCTION_PARAMS: LazyLock<Params> =
    LazyLock::new(|| Params::new(vec![Param::named("target").summary("Target value to lookup a member")]));

/// Parameters of a boolean used as a function: `true a b` is `a`, `false a b` is `b`
static BOOLEAN_FUNCTION_PARAMS: LazyLock<Params> = LazyLock::new(|| {
    Params::new(vec![
        Param::named("then").summary("The result if this is true").lazy(),
        Param::named("else")
            .summary("The result if this is false")
            .default_value(|| Value::Unit)
            .lazy(),
    ])
});

static NO_PARAMS: LazyLock<Params> = LazyLock::new(Params::default);

/// Only problems with the arguments themselves are reported at the call; errors evaluating them propagate
fn argument_error_at_call(error: MashError, call_site: &Option<Location>) -> MashError {
    if error.kind == ErrorKind::Argument {
        error.through_call(None, call_site.clone())
    } else {
        error
    }
}

impl Ctx {
    pub(crate) fn invocation(&mut self, e: &E, function: &E, args: &[Arg], scope: &Scope) -> R<Value> {
        let arguments = self.arguments(args, scope);
        let call_site = self.loc(e.range);
        match &function.kind {
            ExprKind::Mish {
                command,
                args: words,
                redirects,
                capture_output,
            } => {
                let bound = bind(self, &super::mish::MISH_PARAMS, &arguments, &mut no_expr_defaults)
                    .map_err(|e| argument_error_at_call(e, &call_site))?;
                let stdin = bound.values[0].clone().unwrap_or(Value::Null);
                self.mish(function, command, words, redirects, *capture_output, Some(stdin), scope)
            }
            ExprKind::Member {
                target,
                name,
                null_safe,
                name_range,
            } => {
                let this_target = matches!(target.kind, ExprKind::This);
                let target = self.eval(target, scope)?;
                let member_location = self.loc(name_range.or(function.range));
                let result = self.member(&target, name, *null_safe, false, this_target, member_location)?;
                let function_location = self.loc(function.range);
                if result.vectorised
                    && let Value::List(functions) = &result.value
                {
                    let mut results = vec![];
                    for f in functions.snapshot() {
                        results.push(self.call(&f, &arguments, function_location.clone(), call_site.clone())?);
                    }
                    return Ok(Value::list(results));
                }
                self.call(&result.value, &arguments, function_location, call_site)
            }
            _ => {
                let f = self.simple_eval(function, scope)?;
                self.call(&f, &arguments, self.loc(function.range), call_site)
            }
        }
    }

    /// Arguments as written in a call, not yet evaluated
    pub(crate) fn arguments(&self, args: &[Arg], scope: &Scope) -> Vec<Argument> {
        let suspend = |expr: &E| Suspended::Expr {
            expr: expr.clone(),
            scope: scope.clone(),
            unit: self.unit.clone(),
        };
        args.iter()
            .map(|arg| match arg {
                Arg::Positional(e) => Argument::Positional {
                    value: suspend(e),
                    location: self.loc(e.range),
                },
                Arg::ShortFlags { flags, range } => Argument::ShortFlags {
                    flags: flags.clone(),
                    location: self.loc(*range),
                },
                Arg::LongFlag { name, value, range } => Argument::LongFlag {
                    name: name.clone(),
                    value: value.as_ref().map(suspend),
                    location: self.loc(*range),
                },
            })
            .collect()
    }

    /// Call a function with argument values (for builtins calling back into Mash)
    pub fn call_values(&mut self, f: &Value, args: Vec<Value>) -> R<Value> {
        let args: Vec<Argument> = args.into_iter().map(Argument::value).collect();
        self.call(f, &args, None, None)
    }

    /// Call anything callable: functions, bound methods, classes (their constructor), strings (`'name' x` is
    /// `x.name`) and booleans (`cond a b` is if/else).
    pub fn call(
        &mut self,
        f: &Value,
        args: &[Argument],
        function_location: Option<Location>,
        call_site: Option<Location>,
    ) -> R<Value> {
        match f {
            Value::Function(function) => self.call_function(function, args, call_site),
            Value::Builtin(builtin) => self.call_builtin(builtin.0, args, call_site),
            Value::BoundMethod(method) => self.call_bound_method(method, args, call_site),
            Value::Str(s) => {
                let bound = bind(self, &STRING_FUNCTION_PARAMS, args, &mut no_expr_defaults)
                    .map_err(|e| argument_error_at_call(e, &call_site))?;
                let target = bound.values[0].clone().unwrap_or(Value::Null);
                let result = (|| {
                    let member = self.member(&target, s.as_str(), false, true, false, None)?;
                    self.invoke_nullary(member.value, call_site.clone())
                })();
                result.map_err(|e| e.through_call(None, call_site.clone()))
            }
            Value::Bool(b) => {
                let bound = bind(self, &BOOLEAN_FUNCTION_PARAMS, args, &mut no_expr_defaults)
                    .map_err(|e| argument_error_at_call(e, &call_site))?;
                let branch = bound.values[if *b { 0 } else { 1 }].clone().unwrap_or(Value::Unit);
                let result = match branch {
                    Value::Unit => Ok(Value::Unit),
                    f => self.call(&f, &[], None, None),
                };
                result.map_err(|e| e.through_call(None, call_site.clone()))
            }
            Value::Class(class) => match class {
                ClassRef::Builtin(c) => match c.static_method("new") {
                    Some(constructor) => self.call_builtin(constructor, args, call_site),
                    None => Err(not_callable(f, function_location)),
                },
                ClassRef::User(c) => self.call_constructor(c, args, call_site),
            },
            _ => Err(not_callable(f, function_location)),
        }
    }

    /// If `value` is a function or bound method that can be called with no arguments, call it (Scala
    /// `invokeNullaryFunctions`); otherwise return it as it is.
    pub fn invoke_nullary(&mut self, value: Value, location: Option<Location>) -> R<Value> {
        let nullary = match &value {
            Value::Function(_) | Value::Builtin(_) | Value::BoundMethod(_) => {
                self.params_of(&value).is_some_and(Params::allows_nullary)
            }
            _ => false,
        };
        if nullary {
            self.call(&value, &[], None, location)
        } else {
            Ok(value)
        }
    }

    /// The parameters of something callable
    pub fn params_of<'v>(&self, f: &'v Value) -> Option<&'v Params> {
        match f {
            Value::Function(function) => Some(match function.kind() {
                FunctionKind::Closure { params, .. } => params,
                FunctionKind::Constructor(class) => class.params(),
                FunctionKind::Suspended(_) => &NO_PARAMS,
                FunctionKind::SystemCommand(_) => &super::mish::SYSTEM_COMMAND_PARAMS,
            }),
            Value::Builtin(builtin) => Some(builtin.0.params()),
            Value::BoundMethod(method) => Some(match method.method() {
                MethodRef::Builtin { method, .. } => method.params(),
                MethodRef::User { class, index } => &class.methods()[*index].params,
            }),
            Value::Str(_) => Some(&STRING_FUNCTION_PARAMS),
            Value::Bool(_) => Some(&BOOLEAN_FUNCTION_PARAMS),
            _ => None,
        }
    }

    fn call_function(&mut self, function: &Function, args: &[Argument], call_site: Option<Location>) -> R<Value> {
        match function.kind() {
            FunctionKind::Closure { def, params, scope } => {
                let kind = if def.name.is_some() {
                    ScopeKind::Full
                } else {
                    ScopeKind::Leaky
                };
                self.call_closure(
                    def,
                    params,
                    args,
                    call_site,
                    |vars, safe| scope.child(kind, vars, safe),
                    scope,
                )
            }
            FunctionKind::Constructor(class) => self.call_constructor(class, args, call_site),
            FunctionKind::Suspended(suspended) => {
                bind(self, &NO_PARAMS, args, &mut no_expr_defaults)
                    .map_err(|e| argument_error_at_call(e, &call_site))?;
                suspended
                    .resolve(self, false)
                    .map_err(|e| e.through_call(None, call_site))
            }
            FunctionKind::SystemCommand(command) => {
                let bound = bind(self, &super::mish::SYSTEM_COMMAND_PARAMS, args, &mut no_expr_defaults)
                    .map_err(|e| argument_error_at_call(e, &call_site))?;
                let args = bound.values[0].clone().unwrap_or(Value::Null);
                self.call_system_command(command, args)
                    .map_err(|e| e.through_call(None, call_site))
            }
        }
    }

    /// Call a user-defined function or method: bind the arguments (defaults are evaluated in `param_scope`),
    /// then evaluate the body in a new scope made by `body_scope`.
    fn call_closure(
        &mut self,
        def: &Rc<FnDef>,
        params: &Params,
        args: &[Argument],
        call_site: Option<Location>,
        body_scope: impl FnOnce(Object, Vec<Rc<str>>) -> Scope,
        param_scope: &Scope,
    ) -> R<Value> {
        let bound = bind(self, params, args, &mut |ctx, i| {
            let default = def.params[i]
                .default
                .as_ref()
                .expect("parameter with an expression default");
            ctx.with_unit(def.unit.clone(), |ctx| ctx.eval(default, param_scope))
        })
        .map_err(|e| argument_error_at_call(e, &call_site))?;
        let vars = Object::new();
        let mut safe_names = vec![];
        for ((param, def_param), value) in params.params.iter().zip(&def.params).zip(bound.values) {
            let value = value.unwrap_or(Value::Null);
            let simple = !param.variadic && !param.named_args && !param.all_args;
            match &def_param.pattern {
                Some(pattern) if simple => self
                    .bind_pattern(pattern, &value, &mut |name, v| vars.set_str(name, v), None)
                    .map_err(|e| argument_error_at_call(e, &call_site))?,
                _ => {
                    if let Some(name) = &def_param.name {
                        vars.set_str(name, value);
                    }
                }
            }
            if param.safe {
                safe_names.extend(def_param.name.clone());
            }
        }
        let scope = body_scope(vars, safe_names);
        self.enter_call(&call_site)?;
        let result = self.with_unit(def.unit.clone(), |ctx| ctx.eval(&def.body, &scope));
        self.depth -= 1;
        let name = def
            .name
            .as_ref()
            .map(|n| n.to_string())
            .unwrap_or_else(|| "anonymous".into());
        result.map_err(|e| e.through_call(Some(name), call_site))
    }

    fn enter_call(&mut self, call_site: &Option<Location>) -> R<()> {
        if self.depth >= self.max_depth {
            return Err(MashError::at(
                format!(
                    "Maximum call depth ({}) exceeded, possibly due to infinite recursion",
                    self.max_depth
                ),
                call_site.clone(),
            ));
        }
        self.depth += 1;
        Ok(())
    }

    fn call_builtin(
        &mut self,
        f: &'static dyn BuiltinFunction,
        args: &[Argument],
        call_site: Option<Location>,
    ) -> R<Value> {
        let params = f.params();
        let bound =
            bind(self, params, args, &mut no_expr_defaults).map_err(|e| argument_error_at_call(e, &call_site))?;
        let args = Args::new(params, bound);
        f.call(self, &args)
            .map_err(|e| e.through_call(Some(f.name().to_string()), call_site))
    }

    fn call_bound_method(
        &mut self,
        bound_method: &BoundMethod,
        args: &[Argument],
        call_site: Option<Location>,
    ) -> R<Value> {
        let target = bound_method.target();
        match bound_method.method() {
            MethodRef::Builtin { method, .. } => {
                let method: &'static dyn BuiltinMethod = *method;
                let params = method.params();
                let bound = bind(self, params, args, &mut no_expr_defaults)
                    .map_err(|e| argument_error_at_call(e, &call_site))?;
                let args = Args::new(params, bound);
                method
                    .call(self, target, &args)
                    .map_err(|e| e.through_call(Some(method.name().to_string()), call_site))
            }
            MethodRef::User { class, index } => {
                let method = &class.methods()[*index];
                // Defaults see `this`, as the body does
                let param_scope = class.scope().child_with_this(Object::new(), vec![], target.clone());
                let body_scope = |vars, safe| class.scope().child_with_this(vars, safe, target.clone());
                self.call_closure(&method.def, &method.params, args, call_site, body_scope, &param_scope)
            }
        }
    }

    /// `Point 1 2`: an object of the class, with a field for each parameter (Scala `NewStaticMethod`)
    fn call_constructor(&mut self, class: &Class, args: &[Argument], call_site: Option<Location>) -> R<Value> {
        let def = class.def().clone();
        let bound = bind(self, class.params(), args, &mut |ctx, i| {
            let default = def.params[i]
                .default
                .as_ref()
                .expect("parameter with an expression default");
            ctx.with_unit(def.unit.clone(), |ctx| ctx.eval(default, class.scope()))
        })
        .map_err(|e| argument_error_at_call(e, &call_site))?;
        let object = Object::with_class(Some(ClassRef::User(class.clone())));
        for (def_param, value) in def.params.iter().zip(bound.values) {
            let value = value.unwrap_or(Value::Null);
            match (&def_param.pattern, &def_param.name) {
                (Some(pattern), None) => self
                    .bind_pattern(pattern, &value, &mut |name, v| object.set_str(name, v), None)
                    .map_err(|e| argument_error_at_call(e, &call_site))?,
                (_, Some(name)) => object.set_str(name, value),
                (None, None) => object.set_str("anonymousField", value),
            }
        }
        Ok(Value::Object(object))
    }
}

fn no_expr_defaults(_: &mut Ctx, _: usize) -> R<Value> {
    unreachable!("builtins don't have expression defaults")
}

fn not_callable(f: &Value, location: Option<Location>) -> MashError {
    MashError::at(format!("Value of type {} is not callable", f.type_name()), location)
}
