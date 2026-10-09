//! Fields and methods of an object class as methods of a tag class for its names (Scala `MemberLifter`): e.g.
//! a commit hash's `author` looks up the commit and gives its `author` field.

use crate::complete::CompletionSpec;
use crate::eval::{Ctx, MashError, R};
use crate::function::{Args, BuiltinMethod, Params};
use crate::infer::{Inferencer, Type, TypedArgs};
use crate::value::{BuiltinClass, Field, Object, Value};

/// Finds the full object for a name (the target, a tagged string)
pub(crate) type Lookup = fn(&mut Ctx, &str) -> R<Object>;

fn name_of(target: &Value) -> R<&str> {
    target
        .as_str()
        .ok_or_else(|| MashError::new(format!("Expected a String, but was a {}", target.type_name())))
}

/// A field of `class`, as a method of the tag class.
pub(crate) struct LiftedField {
    class: &'static BuiltinClass,
    name: &'static str,
    lookup: Lookup,
}

impl LiftedField {
    pub(crate) const fn new(class: &'static BuiltinClass, name: &'static str, lookup: Lookup) -> Self {
        LiftedField { class, name, lookup }
    }

    fn field(&self) -> &'static Field {
        self.class
            .fields
            .iter()
            .find(|f| f.name == self.name)
            .unwrap_or_else(|| panic!("no field {} in {}", self.name, self.class.full_name()))
    }
}

impl BuiltinMethod for LiftedField {
    fn name(&self) -> &'static str {
        self.name
    }

    fn params(&self) -> &Params {
        static EMPTY: Params = Params { params: vec![] };
        &EMPTY
    }

    fn call(&self, ctx: &mut Ctx, target: &Value, _args: &Args) -> R<Value> {
        let object = (self.lookup)(ctx, name_of(target)?)?;
        object
            .get_str(self.name)
            .ok_or_else(|| MashError::new(format!("No field found: {}", self.name)))
    }

    fn infer(&self, _: &mut Inferencer<'_>, _: Option<&Type>, _: &TypedArgs) -> Option<Type> {
        Some((self.field().ty)())
    }

    fn summary(&self) -> &'static str {
        self.field().summary
    }
}

/// A method of `class`, as a method of the tag class: called on the looked-up object.
pub(crate) struct LiftedMethod {
    class: &'static BuiltinClass,
    method: &'static dyn BuiltinMethod,
    lookup: Lookup,
}

impl LiftedMethod {
    pub(crate) const fn new(class: &'static BuiltinClass, method: &'static dyn BuiltinMethod, lookup: Lookup) -> Self {
        LiftedMethod { class, method, lookup }
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
        let object = (self.lookup)(ctx, name_of(target)?)?;
        self.method.call(ctx, &Value::Object(object), args)
    }

    fn infer(&self, inferencer: &mut Inferencer<'_>, _: Option<&Type>, args: &TypedArgs) -> Option<Type> {
        self.method.infer(inferencer, Some(&Type::Instance(self.class)), args)
    }

    fn completion_specs(&self, arg_pos: usize, _: Option<&Type>, args: &TypedArgs) -> Vec<CompletionSpec> {
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
