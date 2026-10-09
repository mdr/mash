//! Invocation assistance (Ctrl-Space): a box with the summary and calling syntax of the function being
//! called (Scala `AssistanceRenderer`, `AssistanceContentGenerator` and `CallingSyntaxRenderer`).

use mash_core::compile::HOLE_VARIABLE_PREFIX;
use mash_core::complete::Assistable;
use mash_core::function::{Param, Params};
use mash_syntax::SyntaxKind;

use super::boxes::{BoxContent, render_box};
use super::highlight::token_style;
use crate::screen::{Dimensions, Line, Style, StyledString};

/// The assistance box for `assistable`
pub fn render_assistance(assistable: &Assistable, size: Dimensions) -> Vec<Line> {
    render_box(&assistance_content(assistable), size)
}

/// The title and lines of the assistance box (Scala `AssistanceContentGenerator`)
pub fn assistance_content(assistable: &Assistable) -> BoxContent {
    let identifier = token_style(SyntaxKind::IDENTIFIER);
    let summary = |s: &str| (!s.is_empty()).then(|| StyledString::plain(s));
    let (title, summary, syntax) = match assistable {
        Assistable::Function(f) => (
            f.name().to_string(),
            summary(f.summary()),
            function_syntax(f.name(), f.params()),
        ),
        Assistable::Method(m) => (
            m.name().to_string(),
            summary(m.summary()),
            method_syntax("target", m.name(), m.params()),
        ),
        Assistable::UserFunction(f) => {
            let name = f.name.as_deref().unwrap_or("f");
            (
                f.name.as_deref().unwrap_or("Anonymous function").to_string(),
                f.doc.as_ref().and_then(|d| summary(&d.summary)),
                function_syntax(name, &f.params),
            )
        }
        Assistable::UserMethod(m) => {
            let name = m.name.as_deref().unwrap_or("method");
            let mut syntax = identifier.apply("target.");
            syntax.append(&identifier.with_bold().apply(name));
            syntax.push_str(" ", Style::DEFAULT);
            syntax.append(&render_params(&m.params));
            (
                m.name.as_deref().unwrap_or("Anonymous method").to_string(),
                m.doc.as_ref().and_then(|d| summary(&d.summary)),
                syntax,
            )
        }
        Assistable::Constructor(class) => {
            let mut syntax = identifier.apply(&class.name);
            syntax.push_str(".", Style::DEFAULT);
            syntax.append(&identifier.with_bold().apply("new"));
            syntax.push_str(" ", Style::DEFAULT);
            syntax.append(&render_params(&class.params));
            (
                "new".to_string(),
                Some(StyledString::plain(&format!("Construct a new {} object", class.name))),
                syntax,
            )
        }
    };
    BoxContent {
        title,
        lines: summary.into_iter().chain([syntax]).collect(),
    }
}

/// `name <arg> --flag` (Scala `CallingSyntaxRenderer.render(f)`)
pub fn function_syntax(name: &str, params: &Params) -> StyledString {
    let mut s = token_style(SyntaxKind::IDENTIFIER).with_bold().apply(name);
    s.push_str(" ", Style::DEFAULT);
    s.append(&render_params(params));
    s
}

/// `target.name <arg>`
pub fn method_syntax(target: &str, name: &str, params: &Params) -> StyledString {
    let identifier = token_style(SyntaxKind::IDENTIFIER);
    let mut s = identifier.apply(target);
    s.push_str(".", Style::DEFAULT);
    s.append(&identifier.with_bold().apply(name));
    if !params.is_empty() {
        s.push_str(" ", Style::DEFAULT);
        s.append(&render_params(params));
    }
    s
}

/// The parameters, as in `<positional> [<optional>] <variadic>... --flag[=<value>] (--long | -s)`
pub fn render_params(params: &Params) -> StyledString {
    let mut s = StyledString::new();
    for (i, param) in params.params.iter().enumerate() {
        if i > 0 {
            s.push_str(" ", Style::DEFAULT);
        }
        s.append(&if param.flag {
            render_flag(param)
        } else {
            render_positional(param)
        });
    }
    s
}

fn render_flag(param: &Param) -> StyledString {
    let identifier = token_style(SyntaxKind::IDENTIFIER);
    let name = param.name.as_deref().unwrap_or(Param::ANONYMOUS_NAME);
    let mut long = token_style(SyntaxKind::LONG_FLAG).apply(&format!("--{name}"));
    if !param.boolean_flag {
        let value = identifier.apply(param.flag_value_name.as_deref().unwrap_or("value"));
        let (open, close) = if param.flag_value_mandatory {
            ("=<", ">")
        } else {
            ("[=<", ">]")
        };
        long.push_str(open, Style::DEFAULT);
        long.append(&value);
        long.push_str(close, Style::DEFAULT);
    }
    match param.short_flag {
        Some(short) => {
            let mut s = StyledString::plain("(");
            s.append(&long);
            s.push_str(" | ", Style::DEFAULT);
            s.push_str(&format!("-{short}"), token_style(SyntaxKind::SHORT_FLAG));
            s.push_str(")", Style::DEFAULT);
            s
        }
        None => long,
    }
}

fn render_positional(param: &Param) -> StyledString {
    let name = param
        .name
        .as_deref()
        .filter(|n| !n.starts_with(HOLE_VARIABLE_PREFIX))
        .unwrap_or(Param::ANONYMOUS_NAME);
    let mut s = StyledString::plain("<");
    s.append(&token_style(SyntaxKind::IDENTIFIER).apply(name));
    s.push_str(">", Style::DEFAULT);
    if param.variadic {
        s.push_str(if param.variadic_at_least_one { "+..." } else { "..." }, Style::DEFAULT);
        s
    } else if param.has_default() {
        let mut optional = StyledString::plain("[");
        optional.append(&s);
        optional.push_str("]", Style::DEFAULT);
        optional
    } else {
        s
    }
}
