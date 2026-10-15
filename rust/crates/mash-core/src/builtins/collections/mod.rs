//! The `collections` namespace: functions on sequences (lists, and strings as sequences of characters), most
//! of which are also `List` methods.

mod functions;
mod grouping;
mod inference;
mod list_methods;
mod ordering;
mod parse;
mod rename;
mod select;
mod seq_like;
mod sequences;
mod transpose;
mod wrappers;

use std::sync::OnceLock;

use crate::complete::CompletionSpec;
use crate::eval::{Ctx, R};
use crate::function::{Args, BuiltinFunction, BuiltinMethod, Params};
use crate::infer::{Inferencer, Type, TypedArg, TypedArgs, ValueInfo};
use crate::value::{BuiltinClass, Value};

use super::core::ANY_CLASS;

pub use functions::*;
pub use grouping::GROUP_CLASS;
pub(crate) use ordering::{SortFunction, attribute_functions, descending_param, natural_order_param, sort_object_by};
pub(crate) use rename::{DefaultFunction, RenameFunction, UpdateFunction};
pub use sequences::*;
pub(crate) use transpose::TransposeFunction;
pub(crate) use wrappers::{
    COUNT_BY_METHOD, COUNTS_METHOD, GROUP_BY_METHOD, LIST_STATIC_METHODS, MAX_BY_METHOD, MAX_METHOD, MIN_BY_METHOD,
    MIN_METHOD, ORDERING_FUNCTIONS, PARSE_METHOD, PARSE_TABLE_METHOD, SCAN_METHOD, SORT_BY_METHOD, SORT_METHOD,
};

pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] = &[&LengthFunction];

pub static LIST_CLASS: BuiltinClass = BuiltinClass {
    summary: "A sequence of objects",
    // In the order of Scala's ListClass: methodised collection functions, then List's own methods
    methods: &[
        &sequences::ALL_METHOD,
        &sequences::ALL_BUT_LAST_METHOD,
        &sequences::ANY_METHOD,
        &sequences::CHUNKED_METHOD,
        &sequences::CONTAINS_METHOD,
        &rename::DEFAULT_METHOD,
        &wrappers::COUNT_BY_METHOD,
        &wrappers::COUNTS_METHOD,
        &sequences::COUNT_MATCHES_METHOD,
        &sequences::EACH_METHOD,
        &sequences::FLATTEN_METHOD,
        &sequences::FLAT_MAP_METHOD,
        &sequences::FIND_METHOD,
        &sequences::FIRST_METHOD,
        &sequences::GREP_METHOD,
        &wrappers::GROUP_BY_METHOD,
        &sequences::INDEX_OF_METHOD,
        &sequences::IS_EMPTY_METHOD,
        &sequences::JOIN_METHOD,
        &sequences::LAST_METHOD,
        &LENGTH_METHOD,
        &sequences::MAP_METHOD,
        &wrappers::MAX_BY_METHOD,
        &wrappers::MAX_METHOD,
        &wrappers::MIN_BY_METHOD,
        &wrappers::MIN_METHOD,
        &sequences::NON_EMPTY_METHOD,
        &sequences::PARTITION_METHOD,
        &wrappers::PARSE_METHOD,
        &wrappers::PARSE_TABLE_METHOD,
        &rename::RENAME_METHOD,
        &sequences::REDUCE_METHOD,
        &sequences::REVERSE_METHOD,
        &sequences::SKIP_METHOD,
        &sequences::SKIP_UNTIL_METHOD,
        &sequences::SKIP_WHILE_METHOD,
        &sequences::SLIDING_METHOD,
        &wrappers::SORT_BY_METHOD,
        &wrappers::SORT_METHOD,
        &sequences::SUM_BY_METHOD,
        &sequences::SUM_METHOD,
        &sequences::TAKE_WHILE_METHOD,
        &wrappers::TRANSPOSE_METHOD,
        &sequences::UNIQUE_METHOD,
        &sequences::UNIQUE_BY_METHOD,
        &rename::UPDATE_METHOD,
        &sequences::WHERE_METHOD,
        &sequences::WHERE_NOT_METHOD,
        &sequences::WHERE_NOT_NULL_METHOD,
        &list_methods::AppendMethod,
        &list_methods::CartesianProductMethod,
        &list_methods::InnerJoinMethod,
        &list_methods::IntersectMethod,
        &list_methods::NaturalJoinMethod,
        &list_methods::PrependMethod,
        &select::SelectOnListMethod,
    ],
    static_methods: LIST_STATIC_METHODS,
    ..BuiltinClass::new("collections", "List", Some(&ANY_CLASS))
};

pub(crate) static LENGTH_METHOD: Methodised = Methodised::new(&LengthFunction, "sequence", &["count"], false);

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

    fn infer(&self, inferencer: &mut Inferencer<'_>, target: Option<&Type>, args: &TypedArgs) -> Option<Type> {
        self.function.infer(inferencer, &with_target(target, args))
    }

    fn completion_specs(&self, arg_pos: usize, target: Option<&Type>, args: &TypedArgs) -> Vec<CompletionSpec> {
        self.function.completion_specs(arg_pos, &with_target(target, args))
    }
}

/// The function's arguments: the method's, plus the target (Scala `FunctionWrappingMethod.updateArgs`)
fn with_target(target: Option<&Type>, args: &TypedArgs) -> TypedArgs {
    let mut args = args.clone();
    args.args.push(TypedArg::Positional(ValueInfo {
        value: None,
        ty: target.cloned(),
    }));
    args
}
