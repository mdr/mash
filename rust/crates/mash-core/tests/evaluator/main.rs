//! Golden evaluator tests ported from `src/test/scala/com/github/mdr/mash/evaluator`: each case evaluates an
//! expression and compares the result with evaluating the expected expression.

mod harness;

mod and_or;
// mod any_methods; // needs builtins not yet ported
// mod arithmetic; // needs builtins not yet ported
mod assignment;
// mod character; // needs builtins not yet ported
// mod classes; // needs builtins not yet ported
// mod comparison; // needs builtins not yet ported
// mod date_time; // needs builtins not yet ported
mod default_arguments;
// mod doc_comments; // needs builtins not yet ported
mod equality;
// mod evaluator; // needs builtins not yet ported
// mod first_last; // needs builtins not yet ported
mod flag_param;
// mod get_class_method; // needs builtins not yet ported
// mod group_by; // needs builtins not yet ported
// mod help; // needs builtins not yet ported
// mod import; // needs builtins not yet ported
// mod join; // needs builtins not yet ported
mod last_param;
// mod list_methods; // needs builtins not yet ported
mod lookup;
// mod map_flatten; // needs builtins not yet ported
// mod min_max; // needs builtins not yet ported
mod named_args;
// mod natural_join; // needs builtins not yet ported
// mod number_methods; // needs builtins not yet ported
// mod object_arithmetic; // needs builtins not yet ported
// mod object_literal; // needs builtins not yet ported
// mod object_methods; // needs builtins not yet ported
mod patterns;
// mod private_methods; // needs builtins not yet ported
// mod safe_member; // needs builtins not yet ported
// mod select; // needs builtins not yet ported
// mod semicolon_inference; // needs builtins not yet ported
// mod sequence_functions; // needs builtins not yet ported
// mod skip_all_but_last; // needs builtins not yet ported
// mod sort; // needs builtins not yet ported
// mod string_interpolation; // needs builtins not yet ported
// mod string_methods; // needs builtins not yet ported
// mod transpose; // needs builtins not yet ported
// mod variadic_parameter; // needs builtins not yet ported
// mod where_; // needs builtins not yet ported
