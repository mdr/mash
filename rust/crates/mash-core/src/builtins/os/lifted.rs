//! Methods of tag classes whose values identify an object (a username a `UserSummary`, a pid a `Process`):
//! the object's fields and methods, looked up afresh on each call (Scala `UsernameClass.liftUserSummaryField`
//! and friends).

use std::sync::LazyLock;

use crate::complete::CompletionSpec;
use crate::eval::{Ctx, MashError, R};
use crate::function::{Args, BuiltinMethod, Params};
use crate::infer::{Inferencer, Type, TypedArgs};
use crate::value::{BuiltinClass, Field, Value};

static NO_PARAMS: LazyLock<Params> = LazyLock::new(Params::default);

/// Finds the object a tagged value identifies
pub(crate) type Resolve = fn(&mut Ctx, &Value) -> R<Value>;

/// A method of `class`, called on the object the target identifies.
pub(crate) struct LiftedMethod {
    pub(crate) class: &'static BuiltinClass,
    pub(crate) method: &'static dyn BuiltinMethod,
    pub(crate) resolve: Resolve,
}

impl std::fmt::Debug for LiftedMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "LiftedMethod({}.{})", self.class.name, self.method.name())
    }
}

impl BuiltinMethod for LiftedMethod {
    fn name(&self) -> &'static str {
        self.method.name()
    }
    fn aliases(&self) -> &'static [&'static str] {
        self.method.aliases()
    }
    fn params(&self) -> &Params {
        self.method.params()
    }
    fn call(&self, ctx: &mut Ctx, target: &Value, args: &Args) -> R<Value> {
        let object = (self.resolve)(ctx, target)?;
        self.method.call(ctx, &object, args)
    }
    fn infer(&self, inferencer: &mut Inferencer<'_>, _target: Option<&Type>, args: &TypedArgs) -> Option<Type> {
        self.method.infer(inferencer, Some(&Type::Instance(self.class)), args)
    }
    fn completion_specs(&self, arg_pos: usize, _target: Option<&Type>, args: &TypedArgs) -> Vec<CompletionSpec> {
        self.method
            .completion_specs(arg_pos, Some(&Type::Instance(self.class)), args)
    }
    fn summary(&self) -> &'static str {
        self.method.summary()
    }
    fn description(&self) -> Option<&'static str> {
        self.method.description()
    }
}

/// A field of `class` (by name), read from the object the target identifies.
pub(crate) struct LiftedField {
    pub(crate) class: &'static BuiltinClass,
    pub(crate) name: &'static str,
    pub(crate) resolve: Resolve,
}

impl LiftedField {
    fn field(&self) -> Option<&'static Field> {
        self.class.fields.iter().find(|f| f.name == self.name)
    }
}

impl std::fmt::Debug for LiftedField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "LiftedField({}.{})", self.class.name, self.name)
    }
}

impl BuiltinMethod for LiftedField {
    fn name(&self) -> &'static str {
        self.name
    }
    fn params(&self) -> &Params {
        &NO_PARAMS
    }
    fn call(&self, ctx: &mut Ctx, target: &Value, _args: &Args) -> R<Value> {
        match (self.resolve)(ctx, target)? {
            Value::Object(object) => object
                .get_str(self.name)
                .ok_or_else(|| MashError::new(format!("No field found: {}", self.name))),
            other => Ok(other),
        }
    }
    fn infer(&self, _inferencer: &mut Inferencer<'_>, _target: Option<&Type>, _args: &TypedArgs) -> Option<Type> {
        self.field().map(|f| (f.ty)())
    }
    fn summary(&self) -> &'static str {
        self.field().map_or("", |f| f.summary)
    }
}

/// An `info` method: the object the target identifies, an instance of `class`.
pub(crate) struct InfoMethod {
    pub(crate) class: &'static BuiltinClass,
    pub(crate) summary: &'static str,
    pub(crate) resolve: Resolve,
}

impl std::fmt::Debug for InfoMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InfoMethod({})", self.class.name)
    }
}

impl BuiltinMethod for InfoMethod {
    fn name(&self) -> &'static str {
        "info"
    }
    fn params(&self) -> &Params {
        &NO_PARAMS
    }
    fn call(&self, ctx: &mut Ctx, target: &Value, _args: &Args) -> R<Value> {
        (self.resolve)(ctx, target)
    }
    fn infer(&self, _inferencer: &mut Inferencer<'_>, _target: Option<&Type>, _args: &TypedArgs) -> Option<Type> {
        Some(Type::Instance(self.class))
    }
    fn summary(&self) -> &'static str {
        self.summary
    }
}
