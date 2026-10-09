//! The `new` static method of builtin classes whose objects are just their fields.

use std::sync::OnceLock;

use crate::eval::{Ctx, R};
use crate::function::{Args, BuiltinFunction, Param, Params};
use crate::infer::{Inferencer, Type, TypedArgs};
use crate::value::{BuiltinClass, ClassRef, Object, Value};

/// A builtin class's `new` static method, with a parameter for each field (Scala `NewStaticMethod`).
pub(crate) struct FieldsConstructor {
    namespace: &'static str,
    class: &'static BuiltinClass,
    summary: &'static str,
    params: OnceLock<Params>,
}

impl FieldsConstructor {
    /// `namespace` is the class's full name; `summary` is "Construct a new <class> object"
    pub(crate) const fn new(namespace: &'static str, class: &'static BuiltinClass, summary: &'static str) -> Self {
        FieldsConstructor {
            namespace,
            class,
            summary,
            params: OnceLock::new(),
        }
    }
}

impl std::fmt::Debug for FieldsConstructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.new", self.namespace)
    }
}

impl BuiltinFunction for FieldsConstructor {
    fn name(&self) -> &'static str {
        "new"
    }

    fn namespace(&self) -> &'static str {
        self.namespace
    }

    fn params(&self) -> &Params {
        self.params.get_or_init(|| {
            Params::new(
                self.class
                    .fields
                    .iter()
                    .map(|field| Param::named(field.name).summary(field.summary))
                    .collect(),
            )
        })
    }

    fn call(&self, _ctx: &mut Ctx, args: &Args) -> R<Value> {
        let object = Object::with_class(Some(ClassRef::Builtin(self.class)));
        for field in self.class.fields {
            object.set_str(field.name, args.value(field.name).clone());
        }
        Ok(Value::Object(object))
    }

    fn infer(&self, _: &mut Inferencer<'_>, _: &TypedArgs) -> Option<Type> {
        Some(Type::Instance(self.class))
    }

    fn summary(&self) -> &'static str {
        self.summary
    }
}
