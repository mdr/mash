//! Arguments at the type level, and binding them to parameters (Scala `TypedArguments`,
//! `TypeParamBindingContext`, `BoundTypeParams`).

use std::rc::Rc;

use super::types::Type;
use crate::ast::Pattern;
use crate::function::{ArgShape, Params, assign};
use crate::value::{List, Value};

/// What is known statically about an argument: possibly its value (if constant), possibly its type.
#[derive(Debug, Clone, Default)]
pub struct ValueInfo {
    pub value: Option<Value>,
    pub ty: Option<Type>,
}

impl ValueInfo {
    pub fn of_type(ty: Type) -> ValueInfo {
        ValueInfo {
            value: None,
            ty: Some(ty),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypedArg {
    Positional(ValueInfo),
    ShortFlags(Vec<char>),
    LongFlag(Rc<str>, Option<ValueInfo>),
}

impl TypedArg {
    /// The value an argument gives its parameter: flags without a value are `true`
    pub fn value_info(&self) -> ValueInfo {
        match self {
            TypedArg::Positional(info) | TypedArg::LongFlag(_, Some(info)) => info.clone(),
            TypedArg::ShortFlags(_) | TypedArg::LongFlag(_, None) => ValueInfo {
                value: Some(Value::Bool(true)),
                ty: Some(Type::BOOLEAN),
            },
        }
    }

    fn shape(&self) -> ArgShape<'_> {
        match self {
            TypedArg::Positional(_) => ArgShape::Positional,
            TypedArg::ShortFlags(flags) => ArgShape::ShortFlags(flags),
            TypedArg::LongFlag(name, _) => ArgShape::LongFlag(name),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct TypedArgs {
    pub args: Vec<TypedArg>,
}

impl TypedArgs {
    pub fn positional(types: impl IntoIterator<Item = Type>) -> TypedArgs {
        TypedArgs {
            args: types
                .into_iter()
                .map(|t| TypedArg::Positional(ValueInfo::of_type(t)))
                .collect(),
        }
    }

    /// Bind to parameters, forgivingly (Scala `ParameterModel.bindTypes`)
    pub fn bind(&self, params: &Params) -> BoundTypes {
        bind_types(params, self, None)
    }
}

/// Arguments bound to parameters at the type level.
#[derive(Debug, Clone, Default)]
pub struct BoundTypes {
    /// For each parameter (by index), what is known about its argument, if it has one
    pub args: Vec<Option<ValueInfo>>,
    /// Names bound (including by destructuring patterns) and their types
    pub names: Vec<(Rc<str>, Type)>,
    /// For each argument (by position in the call), the parameter it was bound to
    pub arg_params: Vec<Option<usize>>,
}

impl BoundTypes {
    pub fn info(&self, params: &Params, name: &str) -> Option<&ValueInfo> {
        self.args.get(params.index_of(name)?)?.as_ref()
    }

    pub fn ty(&self, params: &Params, name: &str) -> Option<Type> {
        self.info(params, name)?.ty.clone()
    }

    pub fn value(&self, params: &Params, name: &str) -> Option<Value> {
        self.info(params, name)?.value.clone()
    }

    pub fn string_value(&self, params: &Params, name: &str) -> Option<String> {
        self.value(params, name)?.as_str().map(str::to_string)
    }

    pub fn bool_value(&self, params: &Params, name: &str) -> Option<bool> {
        self.value(params, name).map(|v| v.is_truthy())
    }

    pub fn contains(&self, params: &Params, name: &str) -> bool {
        self.info(params, name).is_some()
    }
}

/// Bind typed arguments to parameters. `patterns` gives each parameter's destructuring pattern, for
/// user-defined functions.
pub fn bind_types(params: &Params, args: &TypedArgs, patterns: Option<&[Option<&Pattern>]>) -> BoundTypes {
    let shapes: Vec<ArgShape> = args.args.iter().map(TypedArg::shape).collect();
    let assignment = assign(params, &shapes, true).unwrap_or_else(|_| vec![vec![]; params.params.len()]);
    let mut bound = BoundTypes {
        args: vec![None; params.params.len()],
        names: vec![],
        arg_params: vec![None; args.args.len()],
    };
    for (p, (param, arg_indexes)) in params.params.iter().zip(&assignment).enumerate() {
        for &a in arg_indexes {
            bound.arg_params[a] = Some(p);
        }
        if param.variadic {
            let infos: Vec<ValueInfo> = arg_indexes.iter().map(|&a| args.args[a].value_info()).collect();
            let element = infos.iter().find_map(|i| i.ty.clone()).unwrap_or(Type::ANY);
            let ty = match element {
                Type::Seq(inner) if param.variadic_flatten => Type::Seq(inner),
                t => Type::seq(t),
            };
            let values: Option<Vec<Value>> = infos.iter().map(|i| i.value.clone()).collect();
            if let Some(name) = &param.name {
                bound.names.push((name.as_ref().into(), ty.clone()));
            }
            bound.args[p] = Some(ValueInfo {
                value: values.map(|v| Value::List(List::new(v))),
                ty: Some(ty),
            });
        } else if let Some(&a) = arg_indexes.first() {
            let info = args.args[a].value_info();
            let pattern = patterns.and_then(|ps| ps.get(p).copied().flatten());
            match pattern {
                Some(pattern) => bind_pattern(pattern, info.ty.as_ref(), &mut bound.names),
                None => {
                    if let Some(name) = &param.name {
                        bound
                            .names
                            .push((name.as_ref().into(), info.ty.clone().unwrap_or(Type::ANY)));
                    }
                }
            }
            bound.args[p] = Some(info);
        }
    }
    bound
}

/// Names bound by a destructuring pattern matched against a value of type `ty` (Scala
/// `TypeParamBindingContext.bindPatternParam`)
pub fn bind_pattern(pattern: &Pattern, ty: Option<&Type>, names: &mut Vec<(Rc<str>, Type)>) {
    match pattern {
        Pattern::Ident(name) => names.push((name.clone(), ty.cloned().unwrap_or(Type::ANY))),
        Pattern::Hole => {}
        Pattern::Object(entries) => {
            let field_type = |field: &str| match ty {
                Some(Type::Object(fields)) => fields.get(field).cloned(),
                Some(Type::Instance(class)) => super::field_type(class, field),
                _ => None,
            };
            for entry in entries {
                let field = field_type(&entry.field);
                match &entry.pattern {
                    Some(pattern) => bind_pattern(pattern, field.as_ref(), names),
                    None => names.push((entry.field.clone(), field.unwrap_or(Type::ANY))),
                }
            }
        }
        Pattern::List(patterns) => {
            let element = ty.and_then(Type::element);
            for pattern in patterns {
                bind_pattern(pattern, element, names);
            }
        }
    }
}
