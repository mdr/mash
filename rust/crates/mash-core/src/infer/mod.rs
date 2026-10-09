//! Static type inference for IDE features (completion, invocation assistance, highlighting). Port of
//! `com.github.mdr.mash.inference`.
//!
//! Results go in a side table keyed by node id ([`Inferred`]) rather than in the tree. Inference never fails:
//! where nothing is known, the type is `Any`.

mod constants;
mod inferencer;
mod typed_args;
mod types;
mod value_type;

use mash_syntax::doc_comment::DocComment;

use crate::ast::{self, E, ExprKind};
use crate::eval::Ctx;
use crate::function::{DefaultValue, Param, Params};
use crate::value::BuiltinClass;

pub use inferencer::{Inferencer, Inferred, THIS, TypeInferencer, infer_add, object_fields};
pub use typed_args::{BoundTypes, TypedArg, TypedArgs, ValueInfo, bind_pattern, bind_types};
pub use types::{
    Bindings, BuiltinFunctionType, FunctionBindings, Type, UserClassType, UserFunctionType, extend_bindings,
};
pub use value_type::TypeDetector;

/// Infer types throughout `body`, which will be evaluated in `ctx`'s global scope.
pub fn infer(ctx: &Ctx, body: &E) -> Inferred {
    let mut inferencer = TypeInferencer::default();
    let mut folder = constants::ConstantFolder {
        globals: ctx.global_scope(),
        home: ctx.platform().home_dir().unwrap_or_default(),
        constants: Default::default(),
    };
    folder.fold(body);
    inferencer.inferred.constants = folder.constants;
    let bindings = inferencer.detector.scope_bindings(ctx.global_scope());
    inferencer.infer(body, &bindings, true);
    inferencer.inferred
}

/// The type of a field of a builtin class, if it has one by that name
pub fn field_type(class: &'static BuiltinClass, name: &str) -> Option<Type> {
    class.fields.iter().find(|f| f.name == name).map(|f| (f.ty)())
}

/// A user-defined function's parameters as far as they can be known without evaluating anything: attributes
/// with literal arguments are understood (Scala `Evaluator.parameterModel` without a context)
pub fn static_params(params: &[ast::Param], doc: Option<&DocComment>) -> Params {
    Params::new(
        params
            .iter()
            .map(|param| Param {
                name: param.name.as_ref().map(|n| n.to_string().into()),
                summary: param
                    .name
                    .as_ref()
                    .and_then(|name| doc?.param(name))
                    .map(|comment| comment.summary.clone().into()),
                short_flag: param.attributes.iter().find(|a| &*a.name == "shortFlag").and_then(|a| {
                    match a.args.as_deref()? {
                        [ast::Arg::Positional(e)] => match &e.kind {
                            ExprKind::String { value, .. } if value.chars().count() == 1 => value.chars().next(),
                            _ => None,
                        },
                        _ => None,
                    }
                }),
                default: param.default.as_ref().map(|_| DefaultValue::Expr),
                variadic: param.variadic,
                variadic_at_least_one: param.has_attribute("atLeastOne"),
                variadic_flatten: param.has_attribute("flatten"),
                flag: param.has_attribute("flag"),
                lazy: param.has_attribute("lazy"),
                named_args: param.has_attribute("namedArgs"),
                safe: param.has_attribute("safe"),
                ..Param::default()
            })
            .collect(),
    )
}
