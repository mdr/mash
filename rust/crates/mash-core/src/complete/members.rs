//! Completing members (`x.fo▶`) and bindings (`fo▶`): Scala `MemberCompleter`, `MemberFinder`,
//! `BindingCompleter`.

use std::collections::HashSet;

use mash_syntax::lexer::Token;
use mash_syntax::{SyntaxKind, TextRange, TextSize};

use super::find::find;
use super::{Analysis, Completer, Completion, CompletionResult, CompletionType, analyse, at, replace, tokenise};
use crate::ast::{E, ExprKind};
use crate::builtins::collections::LIST_CLASS;
use crate::builtins::core::*;
use crate::eval::{Ctx, render};
use crate::infer::{BuiltinFunctionType, Type, UserClassType};
use crate::value::{BuiltinClass, ClassRef, Value};

/// A member a value might have (Scala `MemberInfo`)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemberInfo {
    pub name: String,
    pub is_field: bool,
    pub description: Option<String>,
    pub class_name: Option<String>,
    pub vectorised: bool,
    pub is_static: bool,
}

impl MemberInfo {
    fn new(name: impl Into<String>, is_field: bool) -> Self {
        MemberInfo {
            name: name.into(),
            is_field,
            description: None,
            class_name: None,
            vectorised: false,
            is_static: false,
        }
    }

    fn describe(&self) -> String {
        let vectorised = if self.vectorised { "vectorised, " } else { "" };
        let kind = if self.is_field {
            "field"
        } else if self.is_static {
            "static method"
        } else {
            "method"
        };
        let origin = self.class_name.as_ref().map(|c| format!(" in {c}")).unwrap_or_default();
        let description = self.description.as_deref().unwrap_or(&self.name);
        format!("{description} ({vectorised}{kind}{origin})")
    }

    pub fn completion(&self, quoted: bool) -> Completion {
        Completion {
            quoted,
            kind: Some(if self.is_field {
                CompletionType::Field
            } else {
                CompletionType::Method
            }),
            description: Some(self.describe()),
            ..Completion::new(&self.name)
        }
    }
}

fn distinct(members: Vec<MemberInfo>) -> Vec<MemberInfo> {
    let mut seen = HashSet::new();
    members.into_iter().filter(|m| seen.insert(m.name.clone())).collect()
}

fn class_members(class: &'static BuiltinClass) -> Vec<MemberInfo> {
    let mut members = vec![];
    let mut current = Some(class);
    while let Some(class) = current {
        for field in class.fields {
            members.push(MemberInfo {
                description: Some(field.summary.to_string()),
                class_name: Some(class.name.to_string()),
                ..MemberInfo::new(field.name, true)
            });
        }
        for method in class.methods {
            for name in std::iter::once(method.name()).chain(method.aliases().iter().copied()) {
                members.push(MemberInfo {
                    description: Some(method.summary().to_string()),
                    class_name: Some(class.name.to_string()),
                    ..MemberInfo::new(name, false)
                });
            }
        }
        current = class.parent;
    }
    distinct(members)
}

fn user_class_members(class: &UserClassType) -> Vec<MemberInfo> {
    let mut members: Vec<MemberInfo> = class
        .params
        .params
        .iter()
        .filter_map(|p| p.name.as_deref())
        .map(|name| MemberInfo {
            class_name: Some(class.name.to_string()),
            ..MemberInfo::new(name, true)
        })
        .collect();
    for (name, method) in &class.methods {
        if !method.private {
            members.push(MemberInfo {
                class_name: Some(class.name.to_string()),
                ..MemberInfo::new(name.as_ref(), false)
            });
        }
    }
    members.extend(class_members(&OBJECT_CLASS));
    distinct(members)
}

/// The members a value of type `ty` has (Scala `MemberFinder.getMembers`)
pub fn members_of_type(ty: &Type, can_vectorise: bool) -> Vec<MemberInfo> {
    match ty {
        Type::Instance(class) | Type::Generic(class, _) => class_members(class),
        Type::UserClassInstance(class) => user_class_members(class),
        Type::UserClass(class) => {
            let new = MemberInfo {
                class_name: Some(class.name.to_string()),
                is_static: true,
                ..MemberInfo::new("new", false)
            };
            distinct(std::iter::once(new).chain(class_members(&CLASS_CLASS)).collect())
        }
        Type::Tagged { base, tag } => distinct(class_members(tag).into_iter().chain(class_members(base)).collect()),
        Type::BuiltinFunction(_) => class_members(&FUNCTION_CLASS),
        Type::BoundBuiltinMethod(..) => class_members(&BOUND_METHOD_CLASS),
        Type::Object(fields) => {
            let mut members: Vec<MemberInfo> = fields.keys().map(|f| MemberInfo::new(f.as_ref(), true)).collect();
            members.extend(class_members(&OBJECT_CLASS));
            distinct(members)
        }
        Type::Seq(element) => {
            let mut members = class_members(&LIST_CLASS);
            if can_vectorise {
                members.extend(
                    members_of_type(element, false)
                        .into_iter()
                        .map(|m| MemberInfo { vectorised: true, ..m }),
                );
            }
            distinct(members)
        }
        Type::UserFunction(_) | Type::BoundUserMethod(..) => vec![],
    }
}

/// Members of a value known statically: classes (their static methods) and objects (their actual fields)
/// — Scala `MemberFinder.getValueMembers`
pub fn members_of_value(value: &Value) -> Option<Vec<MemberInfo>> {
    match value {
        Value::Class(class) => {
            let statics: Vec<MemberInfo> = match class {
                ClassRef::Builtin(c) => c
                    .static_methods
                    .iter()
                    .map(|m| MemberInfo {
                        description: Some(m.summary().to_string()),
                        class_name: Some(c.name.to_string()),
                        is_static: true,
                        ..MemberInfo::new(m.name(), false)
                    })
                    .collect(),
                ClassRef::User(c) => {
                    vec![MemberInfo {
                        class_name: Some(c.name().to_string()),
                        is_static: true,
                        ..MemberInfo::new("new", false)
                    }]
                }
            };
            Some(distinct(
                statics.into_iter().chain(class_members(&CLASS_CLASS)).collect(),
            ))
        }
        Value::Object(object) => {
            let field_names: Vec<String> = object
                .keys()
                .iter()
                .filter_map(|k| k.as_str().map(str::to_string))
                .collect();
            let class_members = match object.class() {
                Some(ClassRef::Builtin(class)) => class_members(class),
                Some(ClassRef::User(class)) => {
                    let mut members: Vec<MemberInfo> = class
                        .methods()
                        .iter()
                        .filter(|m| !m.private)
                        .map(|m| MemberInfo {
                            class_name: Some(class.name().to_string()),
                            ..MemberInfo::new(m.name(), false)
                        })
                        .collect();
                    members.extend(self::class_members(&OBJECT_CLASS));
                    members
                }
                None => self::class_members(&OBJECT_CLASS),
            };
            let fields = field_names.into_iter().map(|f| MemberInfo::new(f, true));
            Some(distinct(fields.chain(class_members).collect()))
        }
        _ => None,
    }
}

/// The members of the target of a member expression: from its constant value if known, else its type
fn target_members(analysis: &Analysis, target: &E) -> Option<Vec<MemberInfo>> {
    analysis
        .inferred
        .constants
        .get(&target.id)
        .and_then(members_of_value)
        .or_else(|| {
            analysis
                .inferred
                .types
                .get(&target.id)
                .map(|t| members_of_type(t, true))
        })
}

pub(crate) struct MemberCompletion {
    pub(crate) is_member_expr: bool,
    pub(crate) result: Option<CompletionResult>,
    pub(crate) prioritise: bool,
}

/// The member expression (or import) whose name is at `name`, and its target
fn find_member_like(program: &E, name: TextRange) -> Option<(E, E)> {
    let found = find(program, &mut |e| match &e.kind {
        ExprKind::Member { name_range, .. } | ExprKind::Import { name_range, .. } => *name_range == Some(name),
        _ => false,
    })?;
    let target = match &found.kind {
        ExprKind::Member { target, .. } => target.clone(),
        ExprKind::Import { expr, .. } => expr.clone(),
        _ => unreachable!(),
    };
    Some((found, target))
}

/// Complete an identifier as the name of a member
pub(crate) fn complete_identifier(ctx: &mut Ctx, completer: &Completer, identifier: Token) -> MemberCompletion {
    let analysis = analyse(ctx, completer.text, completer.mish);
    let Some((member, target)) = find_member_like(&analysis.program.body, identifier.range) else {
        return MemberCompletion {
            is_member_expr: false,
            result: None,
            prioritise: false,
        };
    };
    let prefix = &completer.text[identifier.range];
    let result = target_members(&analysis, &target).and_then(|members| {
        let completions = members
            .iter()
            .filter(|m| m.name.starts_with(prefix))
            .map(|m| m.completion(false))
            .collect();
        CompletionResult::of(completions, identifier.range)
    });
    let prioritise = matches!(member.kind, ExprKind::Member { .. })
        && !space_before_dot(completer, identifier.range)
        && !matches!(target.kind, ExprKind::Identifier(_) | ExprKind::String { .. });
    MemberCompletion {
        is_member_expr: true,
        result,
        prioritise,
    }
}

/// Whether there's whitespace before the dot that precedes the name at `name`
fn space_before_dot(completer: &Completer, name: TextRange) -> bool {
    let tokens = tokenise(completer.text, completer.mish);
    let Some(index) = tokens.iter().position(|t| t.range == name) else {
        return false;
    };
    let Some(dot) = tokens[..index]
        .iter()
        .rev()
        .find(|t| matches!(t.kind, SyntaxKind::DOT | SyntaxKind::DOT_NULL_SAFE))
    else {
        return false;
    };
    completer.text[..usize::from(dot.range.start())]
        .chars()
        .next_back()
        .is_some_and(char::is_whitespace)
}

const DUMMY: &str = "dummy";

/// Complete members straight after a dot, by inserting a dummy name after it
pub(crate) fn complete_after_dot(ctx: &mut Ctx, completer: &Completer, dot: Token) -> Option<CompletionResult> {
    let dot_text = &completer.text[dot.range];
    let replaced = replace(completer.text, dot.range, &format!("{dot_text}{DUMMY} "));
    let dummy = at(usize::from(dot.range.end()), DUMMY.len());
    let analysis = analyse(ctx, &replaced, completer.mish);
    let (_, target) = find_member_like(&analysis.program.body, dummy)?;
    let members = target_members(&analysis, &target)?;
    CompletionResult::of(
        members.iter().map(|m| m.completion(false)).collect(),
        TextRange::empty(dot.range.end()),
    )
}

// ------------------------------------------------------------------------------------------------ bindings

/// Complete the identifier at `identifier` as a variable: the names in scope there (from type inference), and
/// the globals
pub(crate) fn complete_bindings(
    ctx: &mut Ctx,
    text: &str,
    mish: bool,
    identifier: Token,
    filter_by_prefix: bool,
) -> Option<CompletionResult> {
    let analysis = analyse(ctx, text, mish);
    let expr = find(&analysis.program.body, &mut |e| {
        matches!(e.kind, ExprKind::Identifier(_)) && e.range == Some(identifier.range)
    })?;
    let prefix = if filter_by_prefix { &text[identifier.range] } else { "" };
    let mut completions: Vec<(String, Completion)> = vec![];
    if let Some(bindings) = analysis.inferred.bindings.get(&expr.id) {
        for (name, ty) in bindings.iter() {
            completions.push((name.to_string(), type_completion(name, ty)));
        }
    }
    for (name, value) in ctx.globals().entries() {
        if let Some(name) = name.as_str() {
            completions.retain(|(n, _)| n != name);
            completions.push((name.to_string(), value_completion(name, &value)));
        }
    }
    let completions = completions
        .into_iter()
        .filter(|(n, _)| n.starts_with(prefix))
        .map(|(_, c)| c)
        .collect();
    CompletionResult::of(completions, identifier.range)
}

/// Complete a binding at a position with nothing there yet (by inserting a dummy identifier)
pub(crate) fn complete_bindings_at(ctx: &mut Ctx, text: &str, mish: bool, pos: TextSize) -> Option<CompletionResult> {
    let p = usize::from(pos);
    let replaced = format!("{} {DUMMY} {}", &text[..p], &text[p..]);
    let dummy_start = TextSize::from((p + 1) as u32);
    let identifier = tokenise(&replaced, mish)
        .into_iter()
        .find(|t| t.range.start() <= dummy_start && dummy_start < t.range.end())?;
    let result = complete_bindings(ctx, &replaced, mish, identifier, false)?;
    Some(CompletionResult {
        replace: TextRange::empty(pos),
        ..result
    })
}

fn value_completion(name: &str, value: &Value) -> Completion {
    let (kind, description) = match value {
        Value::Builtin(b) => (CompletionType::Function, b.0.summary().to_string()),
        Value::Function(f) => (CompletionType::Function, f.name().unwrap_or(name).to_string()),
        Value::BoundMethod(m) => (CompletionType::Method, m.name().to_string()),
        Value::Class(ClassRef::Builtin(c)) => (CompletionType::Class, c.summary.to_string()),
        Value::Class(ClassRef::User(c)) => (CompletionType::Class, c.name().to_string()),
        Value::List(_) | Value::Object(_) => (CompletionType::Binding, String::new()),
        other => (CompletionType::Binding, render(other)),
    };
    Completion {
        kind: Some(kind),
        description: (!description.is_empty()).then_some(description),
        ..Completion::new(name)
    }
}

fn type_completion(name: &str, ty: &Type) -> Completion {
    let (kind, description) = match ty {
        Type::BoundBuiltinMethod(_, method) => (CompletionType::Method, Some(method.summary().to_string())),
        Type::BoundUserMethod(_, method) => (
            CompletionType::Method,
            method
                .doc
                .as_ref()
                .map(|d| d.summary.clone())
                .or(method.name.as_deref().map(str::to_string)),
        ),
        Type::BuiltinFunction(BuiltinFunctionType::Builtin(f)) => {
            (CompletionType::Function, Some(f.summary().to_string()))
        }
        Type::UserFunction(f) => (
            CompletionType::Function,
            f.doc
                .as_ref()
                .map(|d| d.summary.clone())
                .or(f.name.as_deref().map(str::to_string)),
        ),
        Type::UserClass(c) => (CompletionType::Class, Some(c.name.to_string())),
        _ => (CompletionType::Binding, None),
    };
    Completion {
        kind: Some(kind),
        description,
        ..Completion::new(name)
    }
}

/// Completions of the members of `ty` that start with `prefix`, quoted (for string arguments naming a member)
pub(crate) fn complete_string_member(ty: &Type, prefix: &str) -> Vec<Completion> {
    members_of_type(ty, true)
        .iter()
        .filter(|m| m.name.starts_with(prefix))
        .map(|m| m.completion(true))
        .collect()
}
