//! How to call a function or method, e.g. `sortBy (--descending | -d) <attributes>+...` (Scala
//! `CallingSyntaxRenderer`). Used by help, and by the REPL's invocation assistance.

use mash_core::compile::HOLE_VARIABLE_PREFIX;
use mash_core::function::Param;
use mash_syntax::SyntaxKind;

use super::highlight::token_style;
use crate::screen::{Style, StyledString};

fn identifier_style() -> Style {
    token_style(SyntaxKind::IDENTIFIER)
}

/// `name <param>…`: calling a function
pub fn function(name: &str, params: &[Param]) -> StyledString {
    identifier_style().with_bold().apply(name) + &StyledString::plain(" ") + &parameters(params)
}

/// `target.name <param>…`: calling a method on a target; `target_name` is an example name for an instance of
/// its class (see [`example_target_name`])
pub fn method(target_name: &str, name: &str, params: &[Param]) -> StyledString {
    let mut s = identifier_style().apply(target_name);
    s.push_str(".", Style::DEFAULT);
    s.append(&identifier_style().with_bold().apply(name));
    if !params.is_empty() {
        s.push_str(" ", Style::DEFAULT);
        s.append(&parameters(params));
    }
    s
}

/// The parameters, separated by spaces
pub fn parameters(params: &[Param]) -> StyledString {
    StyledString::join(params.iter().map(parameter), &StyledString::plain(" "))
}

/// A name for an example instance of a class, as the target of a method call: the class name with a
/// lower-case initial (`String` → `string`) (Scala `exampleTargetName`; `core.Any` uses `value` instead)
pub fn example_target_name(class_name: &str) -> String {
    let mut chars = class_name.chars();
    match chars.next() {
        Some(first) => first.to_lowercase().chain(chars).collect(),
        None => String::new(),
    }
}

fn parameter(param: &Param) -> StyledString {
    if param.flag {
        flag_parameter(param)
    } else {
        positional_parameter(param)
    }
}

fn flag_parameter(param: &Param) -> StyledString {
    let long_form = long_flag(param);
    match param.short_flag {
        Some(short) => {
            let mut s = StyledString::plain("(");
            s.append(&long_form);
            s.push_str(" | ", Style::DEFAULT);
            s.push_str(&format!("-{short}"), token_style(SyntaxKind::SHORT_FLAG));
            s.push_str(")", Style::DEFAULT);
            s
        }
        None => long_form,
    }
}

fn long_flag(param: &Param) -> StyledString {
    let mut s = token_style(SyntaxKind::LONG_FLAG).apply(&format!("--{}", param.name()));
    if !param.boolean_flag {
        let value_name = param.flag_value_name.as_deref().unwrap_or("value");
        let (open, close) = if param.flag_value_mandatory {
            ("=<", ">")
        } else {
            ("[=<", ">]")
        };
        s.push_str(open, Style::DEFAULT);
        s.push_str(value_name, identifier_style());
        s.push_str(close, Style::DEFAULT);
    }
    s
}

fn positional_parameter(param: &Param) -> StyledString {
    let name = match param.name.as_deref() {
        Some(name) if !name.starts_with(HOLE_VARIABLE_PREFIX) => name,
        _ => Param::ANONYMOUS_NAME,
    };
    let mut rendered = StyledString::plain("<");
    rendered.push_str(name, identifier_style());
    rendered.push_str(">", Style::DEFAULT);
    if param.variadic {
        let suffix = if param.variadic_at_least_one { "+..." } else { "..." };
        rendered.push_str(suffix, Style::DEFAULT);
        rendered
    } else if param.has_default() {
        StyledString::plain("[") + &rendered + &StyledString::plain("]")
    } else {
        rendered
    }
}
