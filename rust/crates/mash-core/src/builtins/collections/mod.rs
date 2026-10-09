//! The `collections` namespace: functions on sequences (lists, and strings as sequences of characters), most
//! of which are also `List` methods.

mod functions;

use std::sync::OnceLock;

use crate::eval::{Ctx, R};
use crate::function::{Args, BuiltinFunction, BuiltinMethod, Params};
use crate::value::{BuiltinClass, Value};

use super::core::ANY_CLASS;

pub use functions::*;

/// Functions in `collections` that are also in the global scope
pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] = &[&LengthFunction];

pub static LIST_CLASS: BuiltinClass = BuiltinClass {
    summary: "A sequence of objects",
    methods: &[&LENGTH_METHOD],
    ..BuiltinClass::new("collections", "List", Some(&ANY_CLASS))
};

static LENGTH_METHOD: Methodised = Methodised::new(&LengthFunction, "sequence", &["count"], false);

/// A function exposed as a method: the target becomes the function's sequence argument (Scala
/// `FunctionWrappingMethod`).
pub struct Methodised {
    function: &'static dyn BuiltinFunction,
    /// The function's parameter that receives the target
    target_param: &'static str,
    aliases: &'static [&'static str],
    shy: bool,
    params: OnceLock<(Params, usize)>,
}

impl Methodised {
    pub const fn new(
        function: &'static dyn BuiltinFunction,
        target_param: &'static str,
        aliases: &'static [&'static str],
        shy: bool,
    ) -> Self {
        Methodised {
            function,
            target_param,
            aliases,
            shy,
            params: OnceLock::new(),
        }
    }

    /// The method's parameters (the function's, without the target), and the target's index
    fn params_and_index(&self) -> &(Params, usize) {
        self.params.get_or_init(|| {
            let function_params = self.function.params();
            let index = function_params
                .index_of(self.target_param)
                .unwrap_or_else(|| panic!("{} has no parameter {}", self.function.name(), self.target_param));
            let mut params = function_params.params.clone();
            params.remove(index);
            (Params::new(params), index)
        })
    }
}

impl std::fmt::Debug for Methodised {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "methodise({})", self.function.full_name())
    }
}

impl BuiltinMethod for Methodised {
    fn name(&self) -> &'static str {
        self.function.name()
    }
    fn aliases(&self) -> &'static [&'static str] {
        self.aliases
    }
    fn params(&self) -> &Params {
        &self.params_and_index().0
    }
    fn call(&self, ctx: &mut Ctx, target: &Value, args: &Args) -> R<Value> {
        let index = self.params_and_index().1;
        let args = args.with_extra(self.function.params(), index, target.clone());
        self.function.call(ctx, &args)
    }
    fn summary(&self) -> &'static str {
        self.function.summary()
    }
    fn description(&self) -> Option<&'static str> {
        self.function.description()
    }
    fn shy(&self) -> bool {
        self.shy
    }
}
