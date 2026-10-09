//! Binding arguments to parameters (Scala `GeneralArgBinder` and `ParamBindingContext`).

use std::rc::Rc;

use dumpster::{TraceWith, Visitor};

use super::{DefaultValue, Param, Params};
use crate::ast::{E, SourceUnit};
use crate::eval::{Ctx, Location, MashError, R, suggestion_suffix};
use crate::value::{Function, FunctionKind, Object, Scope, Value};

/// A not-yet-evaluated argument value (Scala `SuspendedMashValue`): an expression from Mash code, or a value
/// from Rust.
#[derive(Clone, Debug)]
pub enum Suspended {
    Expr {
        expr: E,
        scope: Scope,
        unit: Rc<SourceUnit>,
    },
    Value(Value),
}

// SAFETY: visits the scope or value; the expression and unit are plain data (the AST invariant).
#[allow(unsafe_code)]
unsafe impl<V: Visitor> TraceWith<V> for Suspended {
    fn accept(&self, visitor: &mut V) -> Result<(), ()> {
        match self {
            Suspended::Expr { scope, .. } => scope.accept(visitor),
            Suspended::Value(value) => value.accept(visitor),
        }
    }
}

impl Suspended {
    /// Evaluate. If `safe`, a nullary function the expression refers to is not invoked.
    pub fn resolve(&self, ctx: &mut Ctx, safe: bool) -> R<Value> {
        match self {
            Suspended::Expr { expr, scope, unit } => ctx.with_unit(unit.clone(), |ctx| {
                if safe {
                    ctx.simple_eval(expr, scope)
                } else {
                    ctx.eval(expr, scope)
                }
            }),
            Suspended::Value(value) => Ok(value.clone()),
        }
    }
}

/// An argument as given in a call (Scala `EvaluatedArgument`).
#[derive(Clone, Debug)]
pub enum Argument {
    Positional {
        value: Suspended,
        location: Option<Location>,
    },
    /// `-abc`
    ShortFlags {
        flags: Vec<char>,
        location: Option<Location>,
    },
    /// `--name` or `--name=value`
    LongFlag {
        name: Rc<str>,
        value: Option<Suspended>,
        location: Option<Location>,
    },
}

impl Argument {
    pub fn value(value: Value) -> Argument {
        Argument::Positional {
            value: Suspended::Value(value),
            location: None,
        }
    }

    pub fn location(&self) -> Option<&Location> {
        match self {
            Argument::Positional { location, .. }
            | Argument::ShortFlags { location, .. }
            | Argument::LongFlag { location, .. } => location.as_ref(),
        }
    }

    fn is_positional(&self) -> bool {
        matches!(self, Argument::Positional { .. })
    }
}

/// The result of binding: a value for each parameter, in order.
#[derive(Debug)]
pub struct Bound {
    /// None for an omitted `NoArg` parameter, and for an `all_args` parameter (see `all_args`)
    pub values: Vec<Option<Value>>,
    /// Where the argument(s) for each parameter came from
    pub locations: Vec<Option<Location>>,
    /// The arguments for an `all_args` parameter, with values resolved
    pub all_args: Vec<Argument>,
}

/// Bind `args` to `params`, evaluating the arguments (except for lazy parameters). `default` computes the
/// default value of the parameter with the given index, for `DefaultValue::Expr` defaults.
pub fn bind(
    ctx: &mut Ctx,
    params: &Params,
    args: &[Argument],
    default: &mut dyn FnMut(&mut Ctx, usize) -> R<Value>,
) -> R<Bound> {
    let assignment = assign(params, args)?;
    let mut values = Vec::with_capacity(params.params.len());
    let mut locations = Vec::with_capacity(params.params.len());
    let mut all_args = vec![];
    for (i, (param, arg_indexes)) in params.params.iter().zip(&assignment).enumerate() {
        let bound_args: Vec<&Argument> = arg_indexes.iter().map(|&a| &args[a]).collect();
        locations.push(merge_locations(bound_args.iter().filter_map(|a| a.location())));
        let value = if param.variadic {
            Some(bind_variadic(ctx, param, i, &bound_args, default)?)
        } else if param.named_args {
            Some(bind_named_args(ctx, param, &bound_args)?)
        } else if param.all_args {
            for arg in bound_args {
                all_args.push(resolve_argument(ctx, param, arg)?);
            }
            None
        } else {
            match bound_args.as_slice() {
                [] => default_value(ctx, param, i, default)?,
                [arg] => Some(arg_value(ctx, param, arg)?),
                _ => unreachable!("several arguments for parameter {}", param.name()),
            }
        };
        values.push(value);
    }
    Ok(Bound {
        values,
        locations,
        all_args,
    })
}

fn default_value(
    ctx: &mut Ctx,
    param: &Param,
    index: usize,
    default: &mut dyn FnMut(&mut Ctx, usize) -> R<Value>,
) -> R<Option<Value>> {
    Ok(match param.default {
        Some(DefaultValue::Builtin(f)) => Some(f()),
        Some(DefaultValue::Expr) => Some(default(ctx, index)?),
        Some(DefaultValue::NoArg) => None,
        None => unreachable!("no argument for mandatory parameter {}", param.name()),
    })
}

fn bind_variadic(
    ctx: &mut Ctx,
    param: &Param,
    index: usize,
    args: &[&Argument],
    default: &mut dyn FnMut(&mut Ctx, usize) -> R<Value>,
) -> R<Value> {
    if args.is_empty()
        && param.default.is_some()
        && let Some(value) = default_value(ctx, param, index, default)?
    {
        return Ok(value);
    }
    if let [arg @ Argument::LongFlag { value: Some(_), .. }] = args {
        return match arg_value(ctx, param, arg)? {
            list @ Value::List(_) => Ok(list),
            other => Err(MashError::argument(
                format!(
                    "A variadic parameter requires a List argument, but was given a {}",
                    other.type_name()
                ),
                arg.location().cloned(),
            )),
        };
    }
    let mut items = vec![];
    for arg in args {
        match arg_value(ctx, param, arg)? {
            Value::List(xs) if param.variadic_flatten => items.extend(xs.snapshot()),
            value => items.push(value),
        }
    }
    Ok(Value::list(items))
}

fn bind_named_args(ctx: &mut Ctx, param: &Param, args: &[&Argument]) -> R<Value> {
    let object = Object::new();
    let add = |name: &str, value: Value, arg: &Argument| {
        if object.contains_str(name) {
            return Err(MashError::argument(
                format!("Argument '{name}' is provided multiple times"),
                arg.location().cloned(),
            ));
        }
        object.set_str(name, value);
        Ok(())
    };
    for &arg in args {
        match arg {
            Argument::LongFlag { name, value, .. } => {
                let value = match value {
                    Some(v) => v.resolve(ctx, param.safe)?,
                    None => Value::Bool(true),
                };
                add(name, value, arg)?
            }
            Argument::ShortFlags { flags, .. } => {
                for flag in flags {
                    add(&flag.to_string(), Value::Bool(true), arg)?
                }
            }
            Argument::Positional { .. } => {}
        }
    }
    Ok(Value::Object(object))
}

/// The value an argument gives its parameter
fn arg_value(ctx: &mut Ctx, param: &Param, arg: &Argument) -> R<Value> {
    match arg {
        Argument::Positional { value, .. } => resolve(ctx, param, value),
        Argument::LongFlag { value: Some(value), .. } => resolve(ctx, param, value),
        Argument::LongFlag { value: None, .. } | Argument::ShortFlags { .. } => Ok(Value::Bool(true)),
    }
}

fn resolve(ctx: &mut Ctx, param: &Param, value: &Suspended) -> R<Value> {
    if param.lazy {
        Ok(Value::Function(Function::new(FunctionKind::Suspended(value.clone()))))
    } else {
        value.resolve(ctx, param.safe)
    }
}

fn resolve_argument(ctx: &mut Ctx, param: &Param, arg: &Argument) -> R<Argument> {
    let resolve = |ctx: &mut Ctx, v: &Suspended| v.resolve(ctx, param.safe).map(Suspended::Value);
    Ok(match arg {
        Argument::Positional { value, location } => Argument::Positional {
            value: resolve(ctx, value)?,
            location: location.clone(),
        },
        Argument::LongFlag { name, value, location } => Argument::LongFlag {
            name: name.clone(),
            value: value.as_ref().map(|v| resolve(ctx, v)).transpose()?,
            location: location.clone(),
        },
        flags @ Argument::ShortFlags { .. } => flags.clone(),
    })
}

fn merge_locations<'a>(locations: impl Iterator<Item = &'a Location>) -> Option<Location> {
    locations.cloned().reduce(|a, b| Location {
        range: a.range.cover(b.range),
        ..a
    })
}

// ------------------------------------------------------------------------------------------------ assignment

/// Which arguments go to which parameter (by index): Scala `GeneralArgBinder`.
fn assign(params: &Params, args: &[Argument]) -> R<Vec<Vec<usize>>> {
    let ps = &params.params;
    let mut assigned: Vec<Option<Vec<usize>>> = vec![None; ps.len()];
    // Which parameter each argument went to
    let mut arg_param: Vec<Option<usize>> = vec![None; args.len()];
    let add = |assigned: &mut Vec<Option<Vec<usize>>>, arg_param: &mut Vec<Option<usize>>, p: usize, a: usize| {
        assigned[p].get_or_insert_with(Vec::new).push(a);
        arg_param[a] = Some(p);
    };

    // Positional arguments
    let provided_as_flag = |p: &Param| {
        args.iter().any(|arg| match arg {
            Argument::LongFlag { name, .. } => p.name.as_deref() == Some(&**name),
            Argument::ShortFlags { flags, .. } => p.short_flag.is_some_and(|c| flags.contains(&c)),
            Argument::Positional { .. } => false,
        })
    };
    let positional_params: Vec<usize> = (0..ps.len())
        .filter(|&i| !ps[i].flag && !ps[i].named_args && !provided_as_flag(&ps[i]))
        .collect();
    let regular = |i: &usize| !ps[*i].variadic && !ps[*i].all_args;
    let mandatory = |i: &usize| regular(i) && !ps[*i].has_default();
    let optional = |i: &usize| regular(i) && ps[*i].has_default();
    let mut rest = positional_params.as_slice();
    let initial_mandatory = take_while(&mut rest, mandatory);
    let final_mandatory = take_while_back(&mut rest, mandatory);
    let initial_optional = take_while(&mut rest, optional);
    let final_optional = take_while_back(&mut rest, optional);
    let variadic_or_all_args = match rest {
        [p] => Some(*p),
        [] => None,
        _ => return Err(MashError::new("Bad parameters")),
    };

    let positional_args: Vec<usize> = (0..args.len()).filter(|&a| args[a].is_positional()).collect();
    let mut remaining = positional_args.as_slice();
    for &p in initial_mandatory {
        let Some((&a, rest)) = remaining.split_first() else {
            break;
        };
        add(&mut assigned, &mut arg_param, p, a);
        remaining = rest;
    }
    for &p in final_mandatory.iter().rev() {
        let Some((&a, rest)) = remaining.split_last() else {
            break;
        };
        add(&mut assigned, &mut arg_param, p, a);
        remaining = rest;
    }
    for &p in initial_optional {
        let Some((&a, rest)) = remaining.split_first() else {
            break;
        };
        add(&mut assigned, &mut arg_param, p, a);
        remaining = rest;
    }
    for &p in final_optional.iter().rev() {
        let Some((&a, rest)) = remaining.split_last() else {
            break;
        };
        add(&mut assigned, &mut arg_param, p, a);
        remaining = rest;
    }
    if !remaining.is_empty() {
        match variadic_or_all_args {
            Some(p) if ps[p].variadic => {
                for &a in remaining {
                    add(&mut assigned, &mut arg_param, p, a);
                }
            }
            Some(_) => {} // all_args parameters get everything left over, below
            None => {
                let max =
                    initial_mandatory.len() + final_mandatory.len() + initial_optional.len() + final_optional.len();
                let provided = positional_args.len();
                let message = format!(
                    "Too many positional arguments -- {provided} {} provided, but at most {max} {} allowed",
                    if provided == 1 { "was" } else { "were" },
                    if max == 1 { "is" } else { "are" },
                );
                return Err(MashError::argument(
                    message,
                    args[positional_args[max]].location().cloned(),
                ));
            }
        }
    }

    // Flags
    let has_named_args = ps.iter().any(|p| p.named_args);
    let has_all_args = ps.iter().any(|p| p.all_args);
    for (a, arg) in args.iter().enumerate() {
        let names: Vec<String> = match arg {
            Argument::ShortFlags { flags, .. } => flags.iter().map(char::to_string).collect(),
            Argument::LongFlag { name, .. } => vec![name.to_string()],
            Argument::Positional { .. } => continue,
        };
        for name in names {
            match params.index_by_flag(&name) {
                Some(p) if ps[p].named_args => {}
                Some(p) => {
                    if assigned[p].is_some() {
                        let message = format!("{} is provided multiple times", capitalise(&describe(&ps[p])));
                        return Err(MashError::argument(message, arg.location().cloned()));
                    }
                    add(&mut assigned, &mut arg_param, p, a);
                }
                None if has_named_args || has_all_args => {}
                None => {
                    let message = format!(
                        "Unexpected named argument '{name}'{}",
                        suggestion_suffix(&params.flag_names(), &name)
                    );
                    return Err(MashError::argument(message, arg.location().cloned()));
                }
            }
        }
    }

    // Defaults and missing arguments
    for (p, param) in ps.iter().enumerate() {
        if assigned[p].is_some() {
            continue;
        }
        if param.has_default() || (param.variadic && !param.variadic_at_least_one) {
            assigned[p] = Some(vec![]);
        } else if !param.named_args && !param.all_args {
            return Err(MashError::argument(
                format!("Missing mandatory {}", describe(param)),
                None,
            ));
        }
    }

    // Named-arguments and all-arguments parameters get whatever's left
    for (p, param) in ps.iter().enumerate() {
        if param.named_args || param.all_args {
            let leftover: Vec<usize> = (0..args.len())
                .filter(|&a| arg_param[a].is_none() && (param.all_args || !args[a].is_positional()))
                .collect();
            for &a in &leftover {
                arg_param[a] = Some(p);
            }
            assigned[p] = Some(leftover);
        }
    }

    Ok(assigned.into_iter().map(Option::unwrap_or_default).collect())
}

fn take_while<'a>(rest: &mut &'a [usize], f: impl Fn(&usize) -> bool) -> &'a [usize] {
    let n = rest.iter().take_while(|i| f(i)).count();
    let (taken, remaining) = rest.split_at(n);
    *rest = remaining;
    taken
}

fn take_while_back<'a>(rest: &mut &'a [usize], f: impl Fn(&usize) -> bool) -> &'a [usize] {
    let n = rest.iter().rev().take_while(|i| f(i)).count();
    let (remaining, taken) = rest.split_at(rest.len() - n);
    *rest = remaining;
    taken
}

fn describe(param: &Param) -> String {
    match &param.name {
        Some(name) => format!("argument '{name}'"),
        None => "anonymous argument".to_string(),
    }
}

fn capitalise(s: &str) -> String {
    let mut chars = s.chars();
    chars
        .next()
        .map(|c| c.to_uppercase().chain(chars).collect())
        .unwrap_or_default()
}
