//! Function, lambda and class declarations: building their parameter models and evaluating attributes.

use std::rc::Rc;
use std::sync::LazyLock;

use mash_syntax::doc_comment::DocComment;

use crate::ast::{self, Attribute, ClassDef, FnDef};
use crate::function::{Args, Argument, DefaultValue, Param, Params, bind};
use crate::value::{Class, ClassRef, Function, Scope, Str, UserMethod, Value};

use super::{Ctx, MashError, R};

/// Attribute names
mod attributes {
    pub(super) const ALIAS: &str = "alias";
    pub(super) const PRIVATE: &str = "private";
    pub(super) const FLAG: &str = "flag";
    pub(super) const LAZY: &str = "lazy";
    pub(super) const NAMED_ARGS: &str = "namedArgs";
    pub(super) const SHORT_FLAG: &str = "shortFlag";
    pub(super) const AT_LEAST_ONE: &str = "atLeastOne";
    pub(super) const FLATTEN: &str = "flatten";
    pub(super) const SAFE: &str = "safe";
}

static SHORT_FLAG_PARAMS: LazyLock<Params> = LazyLock::new(|| Params::new(vec![Param::named("shortName")]));
static ALIAS_PARAMS: LazyLock<Params> = LazyLock::new(|| Params::new(vec![Param::named("name")]));

impl Ctx {
    pub(crate) fn lambda(&mut self, def: &Rc<FnDef>, scope: &Scope) -> R<Value> {
        let params = self.params_from_def(&def.params, None, scope)?;
        Ok(Value::Function(Function::closure(
            def.clone(),
            Rc::new(params),
            scope.clone(),
        )))
    }

    pub(crate) fn function_decl(&mut self, def: &Rc<FnDef>, scope: &Scope) -> R<Value> {
        // A function's own attributes are evaluated, for any errors, but otherwise unused
        for attribute in &def.attributes {
            for argument in self.arguments(attribute.args.as_deref().unwrap_or_default(), scope) {
                if let Argument::Positional { value, .. } | Argument::LongFlag { value: Some(value), .. } = argument {
                    value.resolve(self, false)?;
                }
            }
        }
        let params = self.params_from_def(&def.params, def.doc.as_ref(), scope)?;
        let function = Value::Function(Function::closure(def.clone(), Rc::new(params), scope.clone()));
        if let Some(name) = &def.name {
            scope.assign(name, function.clone());
        }
        Ok(function)
    }

    pub(crate) fn class_decl(&mut self, def: &Rc<ClassDef>, scope: &Scope) -> R<Value> {
        let params = self.params_from_def(&def.params, def.doc.as_ref(), scope)?;
        let mut methods = vec![];
        for method in &def.methods {
            let mut aliases: Vec<Rc<str>> = vec![];
            for attribute in method.attributes.iter().filter(|a| &*a.name == attributes::ALIAS) {
                let args = self.attribute_args(attribute, &ALIAS_PARAMS, scope)?;
                let alias: Rc<str> = args.get::<Str>("name")?.as_str().into();
                if !aliases.contains(&alias) {
                    aliases.push(alias);
                }
            }
            let private = method.has_attribute(attributes::PRIVATE);
            let method_params = self.params_from_def(&method.params, method.doc.as_ref(), scope)?;
            methods.push(UserMethod {
                def: method.clone(),
                params: Rc::new(method_params),
                aliases,
                private,
            });
        }
        let class = Value::Class(ClassRef::User(Class::new(
            def.clone(),
            Rc::new(params),
            methods,
            scope.clone(),
        )));
        scope.assign(&def.name, class.clone());
        Ok(class)
    }

    /// Evaluate an attribute's arguments, bound to `params`
    fn attribute_args<'p>(&mut self, attribute: &Attribute, params: &'p Params, scope: &Scope) -> R<Args<'p>> {
        let arguments = self.arguments(attribute.args.as_deref().unwrap_or_default(), scope);
        let call_site = self.loc(attribute.range);
        let bound = bind(self, params, &arguments, &mut |_, _| {
            unreachable!("attribute parameters have no defaults")
        })
        .map_err(|e| e.through_call(None, call_site))?;
        Ok(Args::new(params, bound))
    }

    /// The parameter model of a user-defined function, method or class (Scala `Evaluator.parameterModel`)
    fn params_from_def(&mut self, params: &[ast::Param], doc: Option<&DocComment>, scope: &Scope) -> R<Params> {
        if params.iter().filter(|p| p.variadic).count() > 1 {
            return Err(MashError::new("Multiple variadic parameters are not allowed"));
        }
        for (i, param) in params.iter().enumerate() {
            if let Some(name) = &param.name
                && params[..i].iter().any(|p| p.name.as_ref() == Some(name))
            {
                return Err(MashError::at(
                    format!("Duplicate parameter {name}"),
                    self.loc(param.range),
                ));
            }
        }
        let mut result = vec![];
        for param in params {
            let mut short_flag = None;
            if let Some(attribute) = param.attributes.iter().find(|a| &*a.name == attributes::SHORT_FLAG) {
                let args = self.attribute_args(attribute, &SHORT_FLAG_PARAMS, scope)?;
                let name = args.get::<Str>("shortName")?;
                let mut chars = name.as_str().chars();
                match (chars.next(), chars.next()) {
                    (Some(c), None) => short_flag = Some(c),
                    _ => {
                        let message = format!("Short flag must be a single character, but was '{}'", name.as_str());
                        return Err(args.invalid("shortName", &message));
                    }
                }
            }
            let summary = param
                .name
                .as_ref()
                .and_then(|name| doc?.param(name))
                .map(|comment| comment.summary.clone().into());
            result.push(Param {
                name: param.name.as_ref().map(|n| n.to_string().into()),
                summary,
                short_flag,
                default: param.default.as_ref().map(|_| DefaultValue::Expr),
                variadic: param.variadic,
                variadic_at_least_one: param.has_attribute(attributes::AT_LEAST_ONE),
                variadic_flatten: param.has_attribute(attributes::FLATTEN),
                flag: param.has_attribute(attributes::FLAG),
                lazy: param.has_attribute(attributes::LAZY),
                named_args: param.has_attribute(attributes::NAMED_ARGS),
                safe: param.has_attribute(attributes::SAFE),
                ..Param::default()
            });
        }
        Ok(Params::new(result))
    }
}
