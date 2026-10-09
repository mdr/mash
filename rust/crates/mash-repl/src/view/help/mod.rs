//! Help for functions, methods, classes and fields (Scala `view.render.help`).
//!
//! The renderers work on plain descriptions ([`FunctionDoc`], [`MethodDoc`], [`ClassDoc`], [`FieldDoc`]),
//! which are gathered from builtin and user-defined functions and classes by the `*_doc` functions here.
//! (Scala's help also records links from names to their help, for the object browser; they'll come with it.)

mod markup;
mod render;

use std::rc::Rc;

use mash_core::ast::FnDef;
use mash_core::builtins::core::{ANY_CLASS, help::FIELD_HELP_CLASS, help::METHOD_HELP_CLASS};
use mash_core::eval::parent_of;
use mash_core::function::Param;
use mash_core::infer::Type;
use mash_core::value::{BoundMethod, Class, ClassRef, FunctionKind, MethodRef, Value};

pub use markup::render_markup;
pub use render::{class_help, field_help, function_help, method_help};

use super::calling_syntax::example_target_name;
use crate::screen::StyledString;

/// What help shows about a function.
#[derive(Clone, Debug, Default)]
pub struct FunctionDoc {
    /// e.g. `maths.squareRoot`
    pub full_name: String,
    /// Other fully-qualified names
    pub aliases: Vec<String>,
    /// The name it's called by, e.g. `squareRoot`
    pub name: String,
    pub summary: Option<String>,
    pub description: Option<String>,
    pub params: Vec<Param>,
    /// Source code, for user-defined functions
    pub source: Option<String>,
}

/// What help shows about a method.
#[derive(Clone, Debug, Default)]
pub struct MethodDoc {
    pub name: String,
    pub aliases: Vec<String>,
    pub summary: Option<String>,
    pub description: Option<String>,
    /// Full name of the class the method belongs to
    pub class_name: String,
    /// What to call the target in the calling syntax, e.g. `string`
    pub target_name: String,
    pub params: Vec<Param>,
    pub source: Option<String>,
}

/// A field or method, as listed in class help.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct MemberDoc {
    pub name: String,
    pub summary: Option<String>,
}

impl MemberDoc {
    pub fn new(name: &str, summary: Option<&str>) -> MemberDoc {
        MemberDoc {
            name: name.to_string(),
            summary: summary.map(str::to_string),
        }
    }
}

/// What help shows about a class.
#[derive(Clone, Debug, Default)]
pub struct ClassDoc {
    pub full_name: String,
    pub summary: Option<String>,
    pub description: Option<String>,
    /// Full name of the parent class
    pub parent: Option<String>,
    pub fields: Vec<MemberDoc>,
    pub static_methods: Vec<MemberDoc>,
    /// Public methods
    pub methods: Vec<MemberDoc>,
}

/// What help shows about a field of a class.
#[derive(Clone, Debug, Default)]
pub struct FieldDoc {
    pub name: String,
    pub summary: Option<String>,
    pub description: Option<String>,
    /// Full name of the class the field belongs to
    pub class_name: String,
    /// Full name of the field's class, if more specific than `Any`
    pub type_name: Option<String>,
}

/// The help lines for a value that has help: a function, bound method, class, or help object (from
/// `help`/`?`). None for other values.
pub fn help_lines(value: &Value) -> Option<Vec<StyledString>> {
    match value {
        Value::Function(_) | Value::Builtin(_) => function_doc(value).map(|doc| function_help(&doc)),
        Value::BoundMethod(m) => Some(method_help(&bound_method_doc(m))),
        Value::Class(class) => Some(class_help(&class_doc(class))),
        Value::Object(o) => {
            let help_class = o.class()?;
            let name = o.get_str("name")?;
            let Value::Class(owner) = o.get_str("owningClass")? else {
                return None;
            };
            if help_class.is_builtin(&METHOD_HELP_CLASS) {
                method_doc(name.as_str()?, &owner).map(|doc| method_help(&doc))
            } else if help_class.is_builtin(&FIELD_HELP_CLASS) {
                field_doc(name.as_str()?, &owner).map(|doc| field_help(&doc))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Whether `value` is a `MethodHelp` or `FieldHelp` object
pub(crate) fn is_help_object(value: &Value) -> bool {
    matches!(value, Value::Object(o) if o.class().is_some_and(|c|
        c.is_builtin(&METHOD_HELP_CLASS) || c.is_builtin(&FIELD_HELP_CLASS)))
}

fn non_empty(s: &str) -> Option<String> {
    (!s.is_empty()).then(|| s.to_string())
}

fn qualified_name(namespace: Option<&Rc<[Rc<str>]>>, name: &str) -> String {
    match namespace {
        Some(ns) if !ns.is_empty() => format!("{}.{name}", ns.join(".")),
        _ => name.to_string(),
    }
}

/// The documentation of a builtin or user-defined function
pub fn function_doc(value: &Value) -> Option<FunctionDoc> {
    match value {
        Value::Builtin(b) => {
            let f = b.0;
            Some(FunctionDoc {
                full_name: f.full_name(),
                aliases: f.aliases().iter().map(|a| a.to_string()).collect(),
                name: f.name().to_string(),
                summary: non_empty(f.summary()),
                description: f.description().map(str::to_string),
                params: f.params().params.clone(),
                source: None,
            })
        }
        Value::Function(function) => Some(match function.kind() {
            FunctionKind::Closure { def, params, .. } => {
                let name = def.name.as_deref().unwrap_or("anonymous");
                let summary = if def.name.is_none() {
                    Some("anonymous function".to_string())
                } else {
                    def.doc.as_ref().map(|d| d.summary.clone())
                };
                FunctionDoc {
                    full_name: qualified_name(def.namespace.as_ref(), name),
                    name: name.to_string(),
                    summary,
                    description: def.doc.as_ref().and_then(|d| d.description.clone()),
                    params: params.params.clone(),
                    // As in Scala, lambdas don't show their source
                    source: def.name.as_ref().and_then(|_| reindented_source(def)),
                    ..FunctionDoc::default()
                }
            }
            FunctionKind::Constructor(class) => FunctionDoc {
                full_name: "new".into(),
                name: "new".into(),
                summary: Some(format!("Construct a new {} object", class.name())),
                params: class.params().params.clone(),
                ..FunctionDoc::default()
            },
            FunctionKind::Suspended(_) => FunctionDoc {
                full_name: "anonymous".into(),
                name: "anonymous".into(),
                summary: Some("Lazily computed argument".into()),
                ..FunctionDoc::default()
            },
            FunctionKind::SystemCommand(command) => FunctionDoc {
                full_name: command.to_string(),
                name: command.to_string(),
                ..FunctionDoc::default()
            },
        }),
        _ => None,
    }
}

/// The documentation of a class
pub fn class_doc(class: &ClassRef) -> ClassDoc {
    let parent = parent_of(class).map(|p| p.full_name());
    match class {
        ClassRef::Builtin(c) => {
            let mut static_methods: Vec<MemberDoc> = c
                .static_methods
                .iter()
                .map(|m| MemberDoc::new(m.name(), Some(m.summary()).filter(|s| !s.is_empty())))
                .collect();
            static_methods.sort_by(|a, b| a.name.cmp(&b.name));
            let mut methods: Vec<MemberDoc> = c
                .methods
                .iter()
                .filter(|m| !m.private())
                .map(|m| MemberDoc::new(m.name(), Some(m.summary()).filter(|s| !s.is_empty())))
                .collect();
            methods.sort_by(|a, b| a.name.cmp(&b.name));
            ClassDoc {
                full_name: c.full_name(),
                summary: non_empty(c.summary),
                description: c.description.map(str::to_string),
                parent,
                fields: c
                    .fields
                    .iter()
                    .map(|f| MemberDoc::new(f.name, Some(f.summary).filter(|s| !s.is_empty())))
                    .collect(),
                static_methods,
                methods,
            }
        }
        ClassRef::User(c) => {
            let doc = c.def().doc.as_ref();
            let mut methods: Vec<MemberDoc> = c
                .methods()
                .iter()
                .filter(|m| !m.private)
                .map(|m| MemberDoc::new(m.name(), m.def.doc.as_ref().map(|d| d.summary.as_str())))
                .collect();
            methods.sort_by(|a, b| a.name.cmp(&b.name));
            let constructor_summary = format!("Construct a new {} object", c.name());
            ClassDoc {
                full_name: c.full_name(),
                summary: doc.map(|d| d.summary.clone()),
                description: doc.and_then(|d| d.description.clone()),
                parent,
                fields: c
                    .params()
                    .params
                    .iter()
                    .map(|p| MemberDoc::new(p.name(), p.summary.as_deref()))
                    .collect(),
                static_methods: vec![MemberDoc::new("new", Some(&constructor_summary))],
                methods,
            }
        }
    }
}

/// The documentation of the method `name` of `class` (or a parent class)
pub fn method_doc(name: &str, class: &ClassRef) -> Option<MethodDoc> {
    let mut current = Some(class.clone());
    while let Some(class) = current {
        match &class {
            ClassRef::Builtin(c) => {
                if let Some(m) = c.method(name) {
                    return Some(builtin_method_doc(m, &class));
                }
            }
            ClassRef::User(c) => {
                if let Some(index) = c.find_method(name) {
                    return Some(user_method_doc(c, index));
                }
            }
        }
        current = parent_of(&class);
    }
    None
}

fn bound_method_doc(method: &BoundMethod) -> MethodDoc {
    match method.method() {
        MethodRef::Builtin { method, class } => builtin_method_doc(*method, &ClassRef::Builtin(class)),
        MethodRef::User { class, index } => user_method_doc(class, *index),
    }
}

fn builtin_method_doc(method: &'static dyn mash_core::function::BuiltinMethod, class: &ClassRef) -> MethodDoc {
    MethodDoc {
        name: method.name().to_string(),
        aliases: method.aliases().iter().map(|a| a.to_string()).collect(),
        summary: non_empty(method.summary()),
        description: method.description().map(str::to_string),
        class_name: class.full_name(),
        target_name: target_name(class),
        params: method.params().params.clone(),
        source: None,
    }
}

fn user_method_doc(class: &Class, index: usize) -> MethodDoc {
    let m = &class.methods()[index];
    let class_ref = ClassRef::User(class.clone());
    MethodDoc {
        name: m.name().to_string(),
        aliases: m.aliases.iter().map(|a| a.to_string()).collect(),
        summary: m.def.doc.as_ref().map(|d| d.summary.clone()),
        description: m.def.doc.as_ref().and_then(|d| d.description.clone()),
        class_name: class_ref.full_name(),
        target_name: target_name(&class_ref),
        params: m.params.params.clone(),
        source: reindented_source(&m.def),
    }
}

fn target_name(class: &ClassRef) -> String {
    if class.is_builtin(&ANY_CLASS) {
        "value".to_string()
    } else {
        example_target_name(&class.name())
    }
}

/// The documentation of the field `name` of `class`
pub fn field_doc(name: &str, class: &ClassRef) -> Option<FieldDoc> {
    match class {
        ClassRef::Builtin(c) => {
            let field = c.fields.iter().find(|f| f.name == name)?;
            let type_name = match (field.ty)() {
                Type::Instance(c) if !std::ptr::eq(c, &ANY_CLASS) => Some(c.full_name()),
                Type::Tagged { tag, .. } => Some(tag.full_name()),
                _ => None,
            };
            Some(FieldDoc {
                name: field.name.to_string(),
                summary: non_empty(field.summary),
                description: None,
                class_name: c.full_name(),
                type_name,
            })
        }
        ClassRef::User(c) => {
            let param = c.params().params.iter().find(|p| p.name() == name)?;
            Some(FieldDoc {
                name: param.name().to_string(),
                summary: param.summary.as_deref().map(str::to_string),
                description: None,
                class_name: c.full_name(),
                type_name: None,
            })
        }
    }
}

/// The declaration's source text; if it's indented, later lines lose the same indentation (Scala
/// `SourceLocation.reindentedSource`)
fn reindented_source(def: &FnDef) -> Option<String> {
    let range = def.range?;
    let text = &def.unit.text;
    let start = usize::from(range.start());
    let line_start = text[..start].rfind('\n').map_or(0, |i| i + 1);
    let indent = &text[line_start..start];
    let source = def.source()?;
    if indent.is_empty() || !indent.chars().all(|c| c == ' ') {
        return Some(source.to_string());
    }
    let column = indent.chars().count();
    let mut lines = source.split('\n');
    let mut result = lines.next().unwrap_or_default().to_string();
    for line in lines {
        result.push('\n');
        let prefix: String = line.chars().take(column).collect();
        result.push_str(if prefix.chars().all(|c| c == ' ') {
            &line[prefix.len()..]
        } else {
            line
        });
    }
    Some(result)
}
