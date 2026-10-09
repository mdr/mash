//! Invocation assistance (Ctrl-Space): a box with the summary and calling syntax of the function being
//! called (Scala `AssistanceRenderer`, `AssistanceContentGenerator` and `CallingSyntaxRenderer`).

use mash_core::complete::Assistable;
use mash_syntax::SyntaxKind;

use super::boxes::{BoxContent, render_box};
use super::highlight::token_style;
use crate::screen::{Dimensions, Line, Style, StyledString};
use crate::view::calling_syntax;

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
            calling_syntax::function(f.name(), &f.params().params),
        ),
        Assistable::Method(m) => (
            m.name().to_string(),
            summary(m.summary()),
            calling_syntax::method("target", m.name(), &m.params().params),
        ),
        Assistable::UserFunction(f) => {
            let name = f.name.as_deref().unwrap_or("f");
            (
                f.name.as_deref().unwrap_or("Anonymous function").to_string(),
                f.doc.as_ref().and_then(|d| summary(&d.summary)),
                calling_syntax::function(name, &f.params.params),
            )
        }
        Assistable::UserMethod(m) => {
            let name = m.name.as_deref().unwrap_or("method");
            let mut syntax = identifier.apply("target.");
            syntax.append(&identifier.with_bold().apply(name));
            syntax.push_str(" ", Style::DEFAULT);
            syntax.append(&calling_syntax::parameters(&m.params.params));
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
            syntax.append(&calling_syntax::parameters(&class.params.params));
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
