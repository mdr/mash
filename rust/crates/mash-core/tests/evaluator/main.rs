//! Golden evaluator tests ported from `src/test/scala/com/github/mdr/mash/evaluator`: each case evaluates an
//! expression and compares the result with evaluating the expected expression. Cases needing builtins from
//! later milestones are listed in each module's DEFERRED.

mod harness;

mod and_or;
mod any_methods;
mod arithmetic;
mod assignment;
mod character;
mod classes;
mod comparison;
mod core_builtins;
mod date_time;
mod default_arguments;
mod doc_comments;
mod equality;
mod evaluator;
mod first_last;
mod flag_param;
mod get_class_method;
mod group_by;
mod help;
mod import;
mod join;
mod last_param;
mod list_methods;
mod lookup;
mod map_flatten;
mod min_max;
mod named_args;
mod namespaces;
mod natural_join;
mod number_methods;
mod object_arithmetic;
mod object_literal;
mod object_methods;
mod patterns;
mod private_methods;
mod safe_member;
mod select;
mod semicolon_inference;
mod sequence_functions;
mod skip_all_but_last;
mod sort;
mod string_interpolation;
mod string_methods;
mod strings;
mod strings_m3;
mod time;
mod transpose;
mod variadic_parameter;
mod where_;
