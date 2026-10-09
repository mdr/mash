//! Help as lines of styled text (Scala `FunctionHelpRenderer`, `MethodHelpRenderer`, `ClassHelpRenderer`,
//! `FieldHelpRenderer`, `ParameterHelpRenderer`).

use mash_core::function::Param;

use super::markup::render_markup;
use super::{ClassDoc, FieldDoc, FunctionDoc, MemberDoc, MethodDoc};
use crate::screen::{Style, StyledString, default_colours};
use crate::view::calling_syntax;
use crate::view::highlight::highlight;
use crate::view::text;

const INDENT: &str = "    ";

fn section_title_style() -> Style {
    Style::fg(default_colours::YELLOW).with_bold()
}

/// Parameter, field and method names
fn member_style() -> Style {
    Style::fg(default_colours::CYAN).with_bold()
}

fn name_style() -> Style {
    Style::DEFAULT.with_bold()
}

/// What a name in help links to, for the object browser (Scala `Link`).
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum DocLink {
    /// The class a method or field belongs to
    Class,
    /// A class's parent
    Parent,
    /// The class of a field's values
    FieldType,
    /// Help for a field of the class
    Field(String),
    /// Help for a method of the class
    Method(String),
    /// A static method of the class
    StaticMethod(String),
}

/// A link: where it is in the lines of help (a range of characters on a line), and what it links to.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct LinkSpan {
    pub line: usize,
    pub start: usize,
    pub len: usize,
    pub link: DocLink,
}

/// Lines of help, and the links in them.
#[derive(Clone, Debug, Default)]
pub struct HelpLines {
    pub lines: Vec<StyledString>,
    pub links: Vec<LinkSpan>,
}

/// Accumulates the lines of help.
#[derive(Default)]
struct Lines(HelpLines);

impl Lines {
    fn push(&mut self, line: StyledString) {
        self.0.lines.push(line);
    }

    /// Link the name just after the indent on the last line
    fn link(&mut self, name: &str, link: DocLink) {
        self.0.links.push(LinkSpan {
            line: self.0.lines.len() - 1,
            start: INDENT.len(),
            len: name.chars().count(),
            link,
        });
    }

    fn blank(&mut self) {
        self.push(StyledString::new());
    }

    fn title(&mut self, title: &str) {
        self.push(section_title_style().apply(title));
    }

    /// A blank line, then a section title
    fn section(&mut self, title: &str) {
        self.blank();
        self.title(title);
    }

    /// An indented line: `prefix`, then ` - summary` if there is one
    fn summarised(&mut self, prefix: StyledString, summary: Option<&str>) {
        let mut line = StyledString::plain(INDENT) + &prefix;
        if let Some(summary) = summary {
            line.push_str(&format!(" - {summary}"), Style::DEFAULT);
        }
        self.push(line);
    }

    /// e.g. `FUNCTION` / `    maths.squareRoot, sqrt - Take the square root of a number`
    fn name_section(&mut self, title: &str, names: &[String], summary: Option<&str>) {
        self.title(title);
        let names = StyledString::join(names.iter().map(|n| name_style().apply(n)), &StyledString::plain(", "));
        self.summarised(names, summary);
    }

    /// The description's lines (with markup rendered), indented by `level` steps
    fn description(&mut self, description: &str, level: usize) {
        let rendered = render_markup(description);
        let indent = StyledString::plain(&INDENT.repeat(level));
        let mut line = indent.clone();
        for c in &rendered.chars {
            if c.c == '\n' {
                self.push(std::mem::replace(&mut line, indent.clone()));
            } else {
                line.push(*c);
            }
        }
        self.push(line);
    }

    fn description_section(&mut self, description: Option<&str>) {
        if let Some(description) = description {
            self.section("DESCRIPTION");
            self.description(description, 1);
        }
    }

    /// The class a method or field belongs to
    fn class_section(&mut self, class_name: &str) {
        self.section("CLASS");
        self.push(StyledString::plain(&format!("{INDENT}{class_name}")));
        self.link(class_name, DocLink::Class);
    }

    fn calling_syntax_section(&mut self, calling_syntax: StyledString) {
        self.section("CALLING SYNTAX");
        self.push(StyledString::plain(INDENT) + &calling_syntax);
    }

    fn parameters_section(&mut self, params: &[Param]) {
        if params.is_empty() {
            return;
        }
        self.section("PARAMETERS");
        for (i, param) in params.iter().enumerate() {
            if i > 0 {
                self.blank();
            }
            self.parameter(param);
        }
    }

    /// e.g. `    --descending | -d [optional] - Sort results from highest value to lowest`, then any
    /// description
    fn parameter(&mut self, param: &Param) {
        let name = if param.flag {
            format!("--{}", param.name())
        } else {
            param.name().to_string()
        };
        let mut line = member_style().apply(&name);
        if let Some(short) = param.short_flag {
            line.push_str(&format!(" | -{short}"), member_style());
        }
        let qualifiers = qualifiers(param);
        if !qualifiers.is_empty() {
            line.push_str(&format!(" [{}]", qualifiers.join(", ")), Style::DEFAULT);
        }
        self.summarised(line, param.summary.as_deref());
        if let Some(description) = &param.description {
            self.description(description, 2);
        }
    }

    fn source_section(&mut self, source: Option<&str>) {
        if let Some(source) = source {
            self.section("SOURCE");
            for line in text::lines(source) {
                self.push(StyledString::plain(INDENT) + &highlight(line));
            }
        }
    }

    fn members_section(&mut self, title: &str, members: &[MemberDoc], link: fn(String) -> DocLink) {
        if members.is_empty() {
            return;
        }
        self.section(title);
        for member in members {
            self.summarised(member_style().apply(&member.name), member.summary.as_deref());
            self.link(&member.name, link(member.name.clone()));
        }
    }
}

fn qualifiers(param: &Param) -> Vec<&'static str> {
    [
        (param.lazy, "lazy"),
        (param.named_args, "namedArgs"),
        (param.all_args, "allArgs"),
        (param.safe, "safe"),
        (param.has_default(), "optional"),
        (param.variadic, "variadic"),
        (param.variadic_at_least_one, "at least one"),
        (param.variadic_flatten, "flatten"),
    ]
    .into_iter()
    .filter_map(|(set, name)| set.then_some(name))
    .collect()
}

/// Help for a function: names and summary, calling syntax, parameters, description and source
pub fn function_help(doc: &FunctionDoc) -> Vec<StyledString> {
    let mut lines = Lines::default();
    let mut names = vec![doc.full_name.clone()];
    names.extend(doc.aliases.iter().cloned());
    lines.name_section("FUNCTION", &names, doc.summary.as_deref());
    if !doc.params.is_empty() {
        lines.calling_syntax_section(calling_syntax::function(&doc.name, &doc.params));
    }
    lines.parameters_section(&doc.params);
    lines.description_section(doc.description.as_deref());
    lines.source_section(doc.source.as_deref());
    lines.0.lines
}

/// Help for a method: names and summary, class, calling syntax, parameters, description and source
pub fn method_help(doc: &MethodDoc) -> Vec<StyledString> {
    method_help_with_links(doc).lines
}

/// Help for a method, and its link to its class
pub fn method_help_with_links(doc: &MethodDoc) -> HelpLines {
    let mut lines = Lines::default();
    let mut names = vec![doc.name.clone()];
    names.extend(doc.aliases.iter().cloned());
    lines.name_section("METHOD", &names, doc.summary.as_deref());
    lines.class_section(&doc.class_name);
    if !doc.params.is_empty() {
        lines.calling_syntax_section(calling_syntax::method(&doc.target_name, &doc.name, &doc.params));
    }
    lines.parameters_section(&doc.params);
    lines.description_section(doc.description.as_deref());
    lines.source_section(doc.source.as_deref());
    lines.0
}

/// Help for a class: name and summary, description, parent, fields, static methods and methods
pub fn class_help(doc: &ClassDoc) -> Vec<StyledString> {
    class_help_with_links(doc).lines
}

/// Help for a class, and its links to its parent and members
pub fn class_help_with_links(doc: &ClassDoc) -> HelpLines {
    let mut lines = Lines::default();
    lines.title("CLASS");
    lines.summarised(name_style().apply(&doc.full_name), doc.summary.as_deref());
    lines.description_section(doc.description.as_deref());
    if let Some(parent) = &doc.parent {
        lines.section("PARENT");
        lines.push(StyledString::plain(&format!("{INDENT}{parent}")));
        lines.link(parent, DocLink::Parent);
    }
    lines.members_section("FIELDS", &doc.fields, DocLink::Field);
    lines.members_section("STATIC METHODS", &doc.static_methods, DocLink::StaticMethod);
    lines.members_section("METHODS", &doc.methods, DocLink::Method);
    lines.0
}

/// Help for a field: name and summary, class, type and description
pub fn field_help(doc: &FieldDoc) -> Vec<StyledString> {
    field_help_with_links(doc).lines
}

/// Help for a field, and its links to its class and type
pub fn field_help_with_links(doc: &FieldDoc) -> HelpLines {
    let mut lines = Lines::default();
    lines.name_section("FIELD", std::slice::from_ref(&doc.name), doc.summary.as_deref());
    lines.class_section(&doc.class_name);
    if let Some(type_name) = &doc.type_name {
        lines.section("TYPE");
        lines.push(StyledString::plain(&format!("{INDENT}{type_name}")));
        lines.link(type_name, DocLink::FieldType);
    }
    lines.description_section(doc.description.as_deref());
    lines.0
}
