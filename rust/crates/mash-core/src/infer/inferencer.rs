//! The type inferencer (Scala `TypeInferencer`, `InvocationTypeInferencer`, `BinaryOperatorTypeInferencer`),
//! and the `Inferencer` handle that builtins' inference rules use.

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use super::typed_args::{TypedArg, TypedArgs, ValueInfo, bind_pattern, bind_types};
use super::types::*;
use super::value_type::TypeDetector;
use super::{field_type, static_params};
use crate::ast::*;
use crate::builtins::collections::{GROUP_CLASS, LIST_CLASS};
use crate::builtins::core::*;
use crate::function::{Param, Params};
use crate::value::{BuiltinClass, ClassRef, Value};

/// What inference found out about a program, keyed by node.
#[derive(Debug, Default)]
pub struct Inferred {
    /// The type of each expression that was reached (Any if nothing more is known)
    pub types: HashMap<NodeId, Type>,
    /// For a name or member that was invoked automatically because it is nullary, its type before invocation
    pub pre_invocation: HashMap<NodeId, Type>,
    /// The variables in scope at each expression
    pub bindings: HashMap<NodeId, Bindings>,
    /// Statically known values
    pub constants: HashMap<NodeId, Value>,
}

/// The inferencer's state: the side table being built, plus caches.
#[derive(Debug, Default)]
pub struct TypeInferencer {
    pub inferred: Inferred,
    pub(crate) detector: TypeDetector,
    /// Expressions being inferred further up the stack (to stop infinite recursion, e.g. on fixed-point
    /// combinators or recursive functions)
    visiting: HashSet<usize>,
    class_types: HashMap<usize, Rc<UserClassType>>,
}

pub const THIS: &str = "this";

impl TypeInferencer {
    pub fn type_of(&self, e: &E) -> Option<&Type> {
        self.inferred.types.get(&e.id)
    }

    fn constant(&self, e: &E) -> Option<&Value> {
        self.inferred.constants.get(&e.id)
    }

    /// Infer the type of `e` (and its subexpressions). If `immediate_exec`, a reference to a nullary function
    /// has the type of calling it.
    pub fn infer(&mut self, e: &E, bindings: &Bindings, immediate_exec: bool) -> Option<Type> {
        let key = Rc::as_ptr(e) as usize;
        if !self.visiting.insert(key) {
            return self.inferred.types.get(&e.id).cloned();
        }
        let ty = self.infer_kind(e, bindings, immediate_exec);
        self.visiting.remove(&key);
        self.inferred.types.insert(e.id, ty.clone().unwrap_or(Type::ANY));
        self.inferred.bindings.insert(e.id, bindings.clone());
        ty
    }

    fn infer_kind(&mut self, e: &E, bindings: &Bindings, immediate_exec: bool) -> Option<Type> {
        use ExprKind as K;
        match &e.kind {
            K::Hole(_) | K::Pipe { .. } | K::HeadlessMember { .. } | K::Missing => None,
            K::MishFunction(_) => Some(Type::BuiltinFunction(BuiltinFunctionType::SystemCommand)),
            K::Paren(inner) | K::Block(inner) => self.infer(inner, bindings, true),
            K::Literal(Literal::Number(_)) => Some(Type::NUMBER),
            K::Literal(Literal::Bool(_)) => Some(Type::BOOLEAN),
            K::Literal(Literal::Null) => Some(Type::NULL),
            K::String {
                quote: QuoteKind::Double,
                ..
            } => Some(Type::PATH),
            K::String {
                quote: QuoteKind::Single,
                ..
            } => Some(Type::STRING),
            K::Interpolated { parts, .. } => {
                for part in parts {
                    if let InterpolationPart::Expr(e) = part {
                        self.infer(e, bindings, true);
                    }
                }
                Some(Type::PATH)
            }
            K::Minus(inner) => {
                self.infer(inner, bindings, true);
                Some(Type::NUMBER)
            }
            K::BinOp { op, lhs, rhs } => {
                let left = self.infer(lhs, bindings, true);
                let right = self.infer(rhs, bindings, true);
                self.binary_op(left, *op, right, rhs)
            }
            K::ChainedOp { first, rest } => {
                self.infer(first, bindings, true);
                for (_, e) in rest {
                    self.infer(e, bindings, true);
                }
                Some(Type::BOOLEAN)
            }
            K::Member { target, name, .. } => {
                let target_type = self.infer(target, bindings, true)?;
                self.member_lookup(&target_type, name, immediate_exec, Some(e.id), Some(target))
            }
            K::Lookup { target, index } => {
                let target_type = self.infer(target, bindings, true);
                let index_type = self.infer(index, bindings, true);
                if let K::String { value, .. } = &index.kind {
                    return self.member_lookup(&target_type?, value, false, None, None);
                }
                match (target_type?, index_type?) {
                    (Type::Seq(element), index) if index == Type::NUMBER => Some((*element).clone()),
                    (string, index) if string == Type::STRING && index == Type::NUMBER => Some(Type::CHARACTER),
                    (tagged @ Type::Tagged { .. }, index) if tagged.is_string() && index == Type::NUMBER => {
                        Some(tagged)
                    }
                    _ => None,
                }
            }
            K::If { cond, then, else_ } => {
                self.infer(cond, bindings, true);
                let then = self.infer(then, bindings, true);
                let else_ = else_.as_ref().and_then(|e| self.infer(e, bindings, true));
                then.or(else_)
            }
            K::Object(entries) => {
                let mut fields: Vec<(Rc<str>, Type)> = vec![];
                for entry in entries {
                    match entry {
                        ObjectEntry::Full { field, value } => {
                            self.infer(field, bindings, true);
                            let label = match &field.kind {
                                K::Identifier(name) => Some(name.clone()),
                                K::String { value, .. } => Some(value.clone()),
                                _ => None,
                            };
                            let value_type = self.infer(value, bindings, true).unwrap_or(Type::ANY);
                            if let Some(label) = label {
                                fields.push((label, value_type));
                            }
                        }
                        ObjectEntry::Shorthand { name, .. } => {
                            let ty = self.identifier(name, bindings, true, None).unwrap_or(Type::ANY);
                            fields.push((name.clone(), ty));
                        }
                    }
                }
                Some(Type::object(fields))
            }
            K::Identifier(name) => self.identifier(name, bindings, immediate_exec, Some(e.id)),
            K::Mish {
                command,
                args,
                redirects,
                capture_output,
            } => {
                self.infer(command, bindings, true);
                for redirect in redirects {
                    self.infer(&redirect.arg, bindings, true);
                }
                for arg in args {
                    self.infer(arg, bindings, true);
                }
                Some(if *capture_output {
                    Type::Instance(&crate::builtins::os::PROCESS_RESULT_CLASS)
                } else {
                    Type::UNIT
                })
            }
            K::MishInterpolation(inner) => self.infer(inner, bindings, true),
            K::Invocation { function, args, .. } => self.invocation(function, args, bindings),
            K::List(items) => {
                let types: Vec<Option<Type>> = items.iter().map(|item| self.infer(item, bindings, true)).collect();
                Some(Type::seq(types.into_iter().flatten().next().unwrap_or(Type::ANY)))
            }
            K::Assignment { lhs, op, rhs } => {
                let left = self.infer(lhs, bindings, true);
                let right = self.infer(rhs, bindings, true);
                match op {
                    Some(op) => self.binary_op(left, *op, right.clone(), rhs).or(right),
                    None => right,
                }
            }
            K::PatternAssignment { rhs, .. } => self.infer(rhs, bindings, true),
            K::StatementSeq(statements) => self.statements(statements, bindings),
            K::Help(inner) => match self.infer(inner, bindings, false)? {
                Type::BuiltinFunction(_) | Type::UserFunction(_) => Some(Type::FUNCTION),
                Type::BoundBuiltinMethod(..) | Type::BoundUserMethod(..) => {
                    Some(Type::Instance(&crate::builtins::core::help::METHOD_HELP_CLASS))
                }
                _ => None,
            },
            K::Lambda(def) => {
                let preliminary = self.param_bindings(&def.params, bindings);
                self.infer(&def.body, &extend_bindings(bindings, preliminary), true);
                Some(Type::UserFunction(self.function_type(def, bindings, false)))
            }
            K::FunctionDecl(def) => {
                let preliminary = self.param_bindings(&def.params, bindings);
                self.infer(&def.body, &extend_bindings(bindings, preliminary), true);
                Some(Type::UserFunction(self.function_type(
                    def,
                    bindings,
                    def.has_attribute("private"),
                )))
            }
            K::ClassDecl(def) => {
                let field_bindings = self.param_bindings(&def.params, bindings);
                let class_type = self.user_class_type(def, bindings);
                let this = Type::UserClassInstance(class_type.clone());
                let initial = extend_bindings(bindings, field_bindings.into_iter().chain([(Rc::from(THIS), this)]));
                let mut method_bindings = vec![];
                for method in &def.methods {
                    let ty = self.infer_method_decl(method, &initial).unwrap_or(Type::ANY);
                    if let Some(name) = &method.name {
                        method_bindings.push((name.clone(), ty));
                    }
                }
                let updated = extend_bindings(&initial, method_bindings);
                for method in &def.methods {
                    self.infer_method_decl(method, &updated);
                }
                Some(Type::UserClass(class_type))
            }
            K::This => bindings.get(THIS).cloned(),
            K::Import { expr, .. } => {
                self.infer(expr, bindings, true);
                None
            }
        }
    }

    fn infer_method_decl(&mut self, def: &Rc<FnDef>, bindings: &Bindings) -> Option<Type> {
        let preliminary = self.param_bindings(&def.params, bindings);
        self.infer(&def.body, &extend_bindings(bindings, preliminary), true);
        Some(Type::UserFunction(self.function_type(
            def,
            bindings,
            def.has_attribute("private"),
        )))
    }

    fn identifier(
        &mut self,
        name: &str,
        bindings: &Bindings,
        immediate_exec: bool,
        id: Option<NodeId>,
    ) -> Option<Type> {
        let ty = bindings.get(name).cloned();
        if immediate_exec {
            self.immediate_exec(ty, id)
        } else {
            ty
        }
    }

    fn statements(&mut self, statements: &[E], bindings: &Bindings) -> Option<Type> {
        let mut latest = bindings.clone();
        for statement in statements {
            self.infer(statement, &latest, true);
            match &statement.kind {
                ExprKind::Assignment { lhs, .. } => {
                    if let ExprKind::Identifier(name) = &lhs.kind
                        && let Some(ty) = self.type_of(statement).cloned()
                    {
                        latest = extend_bindings(&latest, [(name.clone(), ty)]);
                    }
                }
                ExprKind::PatternAssignment { pattern, rhs } => {
                    let mut names = vec![];
                    bind_pattern(pattern, self.type_of(rhs), &mut names);
                    latest = extend_bindings(&latest, names);
                }
                ExprKind::FunctionDecl(def) => {
                    if let Some(name) = &def.name {
                        let ty = Type::UserFunction(self.function_type(def, &latest, def.has_attribute("private")));
                        latest = extend_bindings(&latest, [(name.clone(), ty)]);
                    }
                }
                ExprKind::ClassDecl(def) => {
                    let ty = Type::UserClass(self.user_class_type(def, &latest));
                    latest = extend_bindings(&latest, [(def.name.clone(), ty)]);
                }
                _ => {}
            }
        }
        match statements.last() {
            Some(last) => self.type_of(last).cloned(),
            None => Some(Type::UNIT),
        }
    }

    /// Types of parameters bound to their default values' types
    fn param_bindings(&mut self, params: &[crate::ast::Param], bindings: &Bindings) -> Vec<(Rc<str>, Type)> {
        let mut names = vec![];
        for param in params {
            let default_type = param.default.as_ref().and_then(|d| self.infer(d, bindings, true));
            match (&param.pattern, &param.name) {
                (Some(pattern), _) if !param.variadic => bind_pattern(pattern, default_type.as_ref(), &mut names),
                (_, Some(name)) => names.push((name.clone(), default_type.unwrap_or(Type::ANY))),
                _ => {}
            }
        }
        names
    }

    fn function_type(&mut self, def: &Rc<FnDef>, bindings: &Bindings, private: bool) -> Rc<UserFunctionType> {
        Rc::new(UserFunctionType {
            name: def.name.clone(),
            doc: def.doc.clone(),
            private,
            params: Rc::new(static_params(&def.params, def.doc.as_ref())),
            def: def.clone(),
            bindings: FunctionBindings::Types(bindings.clone()),
        })
    }

    fn user_class_type(&mut self, def: &Rc<ClassDef>, bindings: &Bindings) -> Rc<UserClassType> {
        let key = Rc::as_ptr(def) as usize;
        if let Some(existing) = self.class_types.get(&key) {
            return existing.clone();
        }
        let mut method_bindings = bindings.clone();
        let mut methods = vec![];
        for method in &def.methods {
            let function = self.function_type(method, &method_bindings, method.has_attribute("private"));
            let mut names: Vec<Rc<str>> = method.name.iter().cloned().collect();
            names.extend(literal_aliases(method));
            for name in names {
                methods.push((name.clone(), function.clone()));
                method_bindings = extend_bindings(&method_bindings, [(name, Type::UserFunction(function.clone()))]);
            }
        }
        let class_type = Rc::new(UserClassType {
            name: def.name.clone(),
            params: Rc::new(static_params(&def.params, def.doc.as_ref())),
            def: Some(def.clone()),
            methods,
        });
        self.class_types.insert(key, class_type.clone());
        class_type
    }

    // ------------------------------------------------------------------------------------------- members

    /// The type of `name` on a value of `target`'s type (Scala `memberLookup`)
    pub fn member_lookup(
        &mut self,
        target: &Type,
        name: &str,
        immediate_exec: bool,
        member_id: Option<NodeId>,
        target_expr: Option<&E>,
    ) -> Option<Type> {
        let raw = match target {
            t if t.is_instance_of(&CLASS_CLASS) => target_expr
                .and_then(|e| self.static_method_type(e, name))
                .or_else(|| lookup_in_class(target, &CLASS_CLASS, name)),
            Type::UserClass(class) => {
                if name == "new" {
                    Some(Type::BuiltinFunction(BuiltinFunctionType::Constructor(class.clone())))
                } else {
                    lookup_in_class(target, &CLASS_CLASS, name)
                }
            }
            Type::Instance(class) => lookup_in_class(target, class, name),
            Type::UserClassInstance(class) => {
                if class.params.index_of(name).is_some() {
                    Some(Type::ANY)
                } else if let Some(method) = class.method(name) {
                    Some(Type::BoundUserMethod(Rc::new(target.clone()), method.clone()))
                } else {
                    lookup_in_class(target, &OBJECT_CLASS, name)
                }
            }
            Type::Tagged { base, tag } => {
                lookup_in_class(target, base, name).or_else(|| lookup_in_class(target, tag, name))
            }
            Type::Seq(element) => lookup_in_class(target, &LIST_CLASS, name).or_else(|| {
                self.member_lookup(element, name, immediate_exec, member_id, target_expr)
                    .map(Type::seq)
            }),
            Type::Object(fields) => fields
                .get(name)
                .cloned()
                .or_else(|| lookup_in_class(target, &OBJECT_CLASS, name)),
            Type::BuiltinFunction(_) | Type::UserFunction(_) => lookup_in_class(target, &FUNCTION_CLASS, name),
            Type::BoundUserMethod(..) | Type::BoundBuiltinMethod(..) => {
                lookup_in_class(target, &BOUND_METHOD_CLASS, name)
            }
            Type::Generic(class, types) => {
                generic_member(class, types, name).or_else(|| lookup_in_class(target, class, name))
            }
        };
        if immediate_exec {
            self.immediate_exec(raw, member_id)
        } else {
            raw
        }
    }

    /// `List.new` etc.: a static method of a class known at compile time
    fn static_method_type(&self, target_expr: &E, name: &str) -> Option<Type> {
        match self.constant(target_expr)? {
            Value::Class(ClassRef::Builtin(class)) => class
                .static_method(name)
                .map(|f| Type::BuiltinFunction(BuiltinFunctionType::Builtin(f))),
            _ => None,
        }
    }

    /// The bindings a function's body sees
    pub(crate) fn resolve(&mut self, bindings: &FunctionBindings) -> Bindings {
        match bindings {
            FunctionBindings::Types(b) => b.clone(),
            FunctionBindings::Scope(scope, extra) => {
                let outer = self.detector.scope_bindings(scope);
                extend_bindings(&outer, extra.iter().map(|(k, v)| (k.clone(), v.clone())))
            }
        }
    }

    /// The type of calling a nullary function or method that is referred to by name (Scala
    /// `inferImmediateExec`)
    fn immediate_exec(&mut self, ty: Option<Type>, id: Option<NodeId>) -> Option<Type> {
        let record = |inferencer: &mut Self, ty: &Type| {
            if let Some(id) = id {
                inferencer.inferred.pre_invocation.insert(id, ty.clone());
            }
        };
        match ty {
            Some(t @ Type::BuiltinFunction(BuiltinFunctionType::Builtin(f))) if f.params().allows_nullary() => {
                record(self, &t);
                f.infer(&mut Inferencer::new(self, Rc::default()), &TypedArgs::default())
            }
            Some(t @ Type::BuiltinFunction(BuiltinFunctionType::Constructor(_))) => {
                let Type::BuiltinFunction(BuiltinFunctionType::Constructor(class)) = &t else {
                    unreachable!()
                };
                if class.params.allows_nullary() {
                    record(self, &t);
                    Some(Type::UserClassInstance(class.clone()))
                } else {
                    Some(t)
                }
            }
            Some(Type::BoundBuiltinMethod(target, method)) if method.params().allows_nullary() => {
                record(self, &Type::BoundBuiltinMethod(target.clone(), method));
                method.infer(
                    &mut Inferencer::new(self, Rc::default()),
                    Some(&target),
                    &TypedArgs::default(),
                )
            }
            Some(Type::UserFunction(f)) if f.params.allows_nullary() => {
                record(self, &Type::UserFunction(f.clone()));
                self.apply_user_function(&f, &TypedArgs::default(), None)
            }
            Some(Type::BoundUserMethod(target, m)) if m.params.allows_nullary() => {
                record(self, &Type::BoundUserMethod(target.clone(), m.clone()));
                self.apply_user_function(&m, &TypedArgs::default(), Some(&target))
            }
            other => other,
        }
    }

    /// The type of a user-defined function's body with arguments of the given types
    fn apply_user_function(&mut self, f: &UserFunctionType, args: &TypedArgs, this: Option<&Type>) -> Option<Type> {
        let patterns: Vec<Option<&Pattern>> = f.def.params.iter().map(|p| p.pattern.as_ref()).collect();
        let bound = bind_types(&f.params, args, Some(&patterns));
        let base = self.resolve(&f.bindings);
        let this = this.map(|t| (Rc::from(THIS), t.clone()));
        let bindings = extend_bindings(&base, bound.names.into_iter().chain(this));
        self.infer(&f.def.body, &bindings, true)
    }

    // ------------------------------------------------------------------------------------------- invocations

    fn invocation(&mut self, function: &E, args: &[Arg], bindings: &Bindings) -> Option<Type> {
        for arg in args {
            match arg {
                Arg::Positional(e) | Arg::LongFlag { value: Some(e), .. } => {
                    self.infer(e, bindings, true);
                }
                _ => {}
            }
        }
        let typed = self.typed_args(args);
        let function_type = self.infer(function, bindings, false)?;
        self.invoke(&function_type, &typed, Some(function), bindings)
    }

    /// Typed arguments from a call's arguments, using what's been inferred about them
    pub fn typed_args(&self, args: &[Arg]) -> TypedArgs {
        let info = |e: &E| ValueInfo {
            value: self.constant(e).cloned(),
            ty: self.type_of(e).cloned(),
        };
        TypedArgs {
            args: args
                .iter()
                .map(|arg| match arg {
                    Arg::Positional(e) => TypedArg::Positional(info(e)),
                    Arg::ShortFlags { flags, .. } => TypedArg::ShortFlags(flags.clone()),
                    Arg::LongFlag { name, value, .. } => TypedArg::LongFlag(name.clone(), value.as_ref().map(info)),
                })
                .collect(),
        }
    }

    /// The type of calling something of type `function_type` (Scala `inferTypeInvocation`)
    pub fn invoke(
        &mut self,
        function_type: &Type,
        args: &TypedArgs,
        function: Option<&E>,
        bindings: &Bindings,
    ) -> Option<Type> {
        match function_type {
            t if t.is_string() => {
                // `'name' x` is `x.name`
                let arg_type = args.args.iter().find_map(|a| match a {
                    TypedArg::Positional(info) => Some(info.ty.clone()),
                    _ => None,
                })??;
                let name = function.and_then(|f| self.constant(f))?.as_str()?.to_string();
                self.member_lookup(&arg_type, &name, true, None, None)
            }
            Type::Seq(element) => match &**element {
                method @ (Type::BoundUserMethod(..) | Type::BoundBuiltinMethod(..)) => {
                    self.invoke(method, args, function, bindings).map(Type::seq)
                }
                element => Some(element.clone()),
            },
            Type::BoundUserMethod(target, method) => self.apply_user_function(method, args, Some(target)),
            Type::BoundBuiltinMethod(target, method) => {
                method.infer(&mut Inferencer::new(self, bindings.clone()), Some(target), args)
            }
            Type::BuiltinFunction(BuiltinFunctionType::Builtin(f)) => {
                f.infer(&mut Inferencer::new(self, bindings.clone()), args)
            }
            Type::BuiltinFunction(BuiltinFunctionType::Constructor(class)) | Type::UserClass(class) => {
                Some(Type::UserClassInstance(class.clone()))
            }
            Type::BuiltinFunction(BuiltinFunctionType::SystemCommand) => None,
            Type::UserFunction(f) => self.apply_user_function(f, args, None),
            t if t.is_instance_of(&CLASS_CLASS) => {
                let constructor = function.and_then(|f| self.static_method_type(f, "new"))?;
                self.invoke(&constructor, args, None, bindings)
            }
            t if *t == Type::BOOLEAN => {
                // `cond a b` is `a` or `b`
                let params = Params::new(vec![
                    Param::named("then"),
                    Param::named("else").default_value(|| Value::Unit),
                ]);
                let bound = args.bind(&params);
                bound.ty(&params, "then").or_else(|| bound.ty(&params, "else"))
            }
            _ => None,
        }
    }

    // ------------------------------------------------------------------------------------------- operators

    fn binary_op(&mut self, left: Option<Type>, op: BinOp, right: Option<Type>, right_expr: &E) -> Option<Type> {
        match op {
            BinOp::Plus => infer_add(left, right),
            BinOp::Multiply => infer_multiply(left, right),
            BinOp::Minus => self.infer_subtract(left, right, right_expr),
            BinOp::Divide => match (&left, &right) {
                (Some(l @ Type::Tagged { .. }), _) if l.is_number() => left,
                (_, Some(r @ Type::Tagged { .. })) if r.is_number() => right,
                _ => Some(Type::NUMBER),
            },
            BinOp::Equals
            | BinOp::NotEquals
            | BinOp::LessThan
            | BinOp::LessThanEquals
            | BinOp::GreaterThan
            | BinOp::GreaterThanEquals => Some(Type::BOOLEAN),
            BinOp::And | BinOp::Or => left.or(right),
        }
    }

    fn infer_subtract(&mut self, left: Option<Type>, right: Option<Type>, right_expr: &E) -> Option<Type> {
        let tagged_number = |t: &Option<Type>| matches!(t, Some(t @ Type::Tagged { .. }) if t.is_number());
        if tagged_number(&left) {
            return left;
        }
        if tagged_number(&right) {
            return right;
        }
        if let Some(fields) = left.as_ref().and_then(object_fields) {
            if right.as_ref().is_some_and(Type::is_string) {
                let name = self.constant(right_expr)?.as_str()?.to_string();
                return Some(Type::object(fields.into_iter().filter(|(k, _)| **k != *name)));
            }
            if let Some(Type::Seq(_)) = &right {
                let Some(Value::List(xs)) = self.constant(right_expr) else {
                    return None;
                };
                let removed: Vec<String> = xs
                    .snapshot()
                    .iter()
                    .filter_map(|v| v.as_str().map(str::to_string))
                    .collect();
                return Some(Type::object(
                    fields.into_iter().filter(|(k, _)| !removed.iter().any(|r| **r == **k)),
                ));
            }
        }
        match (&left, &right) {
            (Some(Type::Seq(_)), _) => left,
            (_, Some(Type::Seq(_))) => right,
            _ => Some(Type::NUMBER),
        }
    }
}

/// Aliases given by `@(alias 'name')` with a literal string
fn literal_aliases(def: &FnDef) -> Vec<Rc<str>> {
    def.attributes
        .iter()
        .filter(|a| &*a.name == "alias")
        .filter_map(|a| match a.args.as_deref()? {
            [Arg::Positional(e)] => match &e.kind {
                ExprKind::String { value, .. } => Some(value.clone()),
                _ => None,
            },
            _ => None,
        })
        .collect()
}

/// A field, or method bound to `target`, of a builtin class or its ancestors
pub(crate) fn lookup_in_class(target: &Type, class: &'static BuiltinClass, name: &str) -> Option<Type> {
    let mut current = Some(class);
    while let Some(class) = current {
        if let Some(ty) = field_type(class, name) {
            return Some(ty);
        }
        if let Some(method) = class.method(name) {
            return Some(Type::BoundBuiltinMethod(Rc::new(target.clone()), method));
        }
        current = class.parent;
    }
    None
}

fn generic_member(class: &'static BuiltinClass, types: &[Type], name: &str) -> Option<Type> {
    if std::ptr::eq(class, &GROUP_CLASS) {
        match (name, types) {
            ("key", [key, _]) => Some(key.clone()),
            ("values", [_, element]) => Some(Type::seq(element.clone())),
            _ => None,
        }
    } else if std::ptr::eq(class, &TIMED_RESULT_CLASS) {
        match (name, types) {
            ("result", [result]) => Some(result.clone()),
            _ => None,
        }
    } else {
        None
    }
}

/// The fields of an object-like type (Scala `getObjectFields`)
pub fn object_fields(ty: &Type) -> Option<Vec<(Rc<str>, Type)>> {
    match ty {
        Type::Instance(class) | Type::Generic(class, _) => Some(
            class
                .fields
                .iter()
                .map(|f| (Rc::from(f.name), field_type(class, f.name).unwrap_or(Type::ANY)))
                .collect(),
        ),
        Type::UserClassInstance(class) => Some(
            class
                .params
                .params
                .iter()
                .filter_map(|p| Some((Rc::from(p.name.as_deref()?), Type::ANY)))
                .collect(),
        ),
        Type::Object(fields) => Some(fields.iter().map(|(k, v)| (k.clone(), v.clone())).collect()),
        _ => None,
    }
}

fn is_object_class(ty: &Type) -> bool {
    match ty {
        Type::Instance(class) => {
            crate::eval::is_subclass_of(&ClassRef::Builtin(class), &ClassRef::Builtin(&OBJECT_CLASS))
        }
        _ => false,
    }
}

/// The type of `left + right` (Scala `BinaryOperatorTypeInferencer.inferTypeAdd`)
pub fn infer_add(left: Option<Type>, right: Option<Type>) -> Option<Type> {
    match (&left, &right) {
        (Some(Type::Seq(l)), Some(Type::Seq(_))) => {
            if l.is_any() {
                right
            } else {
                left
            }
        }
        (Some(l), _) if l.is_string() => left,
        (_, Some(r)) if r.is_string() => right,
        (Some(l), _) if l.is_number() => left,
        (_, Some(r)) if r.is_number() => right,
        (_, Some(r)) if is_object_class(r) || matches!(r, Type::UserClassInstance(_)) => right,
        (Some(l), _) if is_object_class(l) || matches!(l, Type::UserClassInstance(_)) => left,
        _ => {
            let merged = match (
                left.as_ref().and_then(object_fields),
                right.as_ref().and_then(object_fields),
            ) {
                (Some(l), Some(r)) => {
                    let mut fields: indexmap::IndexMap<Rc<str>, Type> = l.into_iter().collect();
                    fields.extend(r);
                    Some(Type::Object(Rc::new(fields)))
                }
                _ => None,
            };
            merged.or(Some(Type::NUMBER))
        }
    }
}

fn infer_multiply(left: Option<Type>, right: Option<Type>) -> Option<Type> {
    let tagged_number = |t: &Type| matches!(t, Type::Tagged { .. }) && t.is_number();
    match (&left, &right) {
        (Some(l), Some(r)) if tagged_number(l) && r.is_number() => left,
        (Some(l), Some(r)) if l.is_number() && r.is_number() => right,
        (Some(l), Some(r)) if l.is_number() && (r.is_string() || matches!(r, Type::Seq(_))) => right,
        (Some(l), Some(r)) if (l.is_string() || matches!(l, Type::Seq(_))) && r.is_number() => left,
        _ => Some(Type::NUMBER),
    }
}

/// What builtins' inference rules get: access to the inferencer, and the bindings in scope at the call.
pub struct Inferencer<'i> {
    pub inferencer: &'i mut TypeInferencer,
    pub bindings: Bindings,
}

impl std::fmt::Debug for Inferencer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Inferencer")
    }
}

impl<'i> Inferencer<'i> {
    pub fn new(inferencer: &'i mut TypeInferencer, bindings: Bindings) -> Self {
        Inferencer { inferencer, bindings }
    }

    /// The type of applying a function (of type `function`) to one argument of type `argument` (Scala
    /// `Inferencer.applyFunction`). `function_value` is the function's value if known (for strings used as
    /// functions).
    pub fn apply_function(&mut self, function: &Type, argument: &Type, function_value: Option<&Value>) -> Option<Type> {
        match function {
            Type::BuiltinFunction(BuiltinFunctionType::Builtin(f)) => f.infer(
                &mut Inferencer::new(self.inferencer, self.bindings.clone()),
                &TypedArgs::positional([argument.clone()]),
            ),
            Type::BoundBuiltinMethod(target, method) => method.infer(
                &mut Inferencer::new(self.inferencer, self.bindings.clone()),
                Some(target),
                &TypedArgs::positional([argument.clone()]),
            ),
            Type::UserFunction(f) => {
                let param = f.params.params.first()?;
                let base = self.inferencer.resolve(&f.bindings);
                let mut names: Vec<(Rc<str>, Type)> =
                    self.bindings.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                if let Some(name) = &param.name {
                    names.push((name.as_ref().into(), argument.clone()));
                }
                let bindings = extend_bindings(&base, names);
                self.inferencer.infer(&f.def.body, &bindings, true)
            }
            Type::BoundUserMethod(target, m) => {
                self.inferencer
                    .apply_user_function(m, &TypedArgs::positional([argument.clone()]), Some(target))
            }
            t if t.is_string() => {
                let name = function_value?.as_str()?.to_string();
                self.inferencer.member_lookup(argument, &name, true, None, None)
            }
            _ => None,
        }
    }

    /// The type of applying a function to two arguments (Scala `Inferencer.applyFunction2`)
    pub fn apply_function2(&mut self, function: &Type, first: &Type, second: &Type) -> Option<Type> {
        let args = TypedArgs::positional([first.clone(), second.clone()]);
        match function {
            Type::BuiltinFunction(BuiltinFunctionType::Builtin(f)) => {
                f.infer(&mut Inferencer::new(self.inferencer, self.bindings.clone()), &args)
            }
            Type::BoundBuiltinMethod(target, method) => method.infer(
                &mut Inferencer::new(self.inferencer, self.bindings.clone()),
                Some(target),
                &args,
            ),
            Type::UserFunction(f) => {
                let base = self.inferencer.resolve(&f.bindings);
                let mut names: Vec<(Rc<str>, Type)> =
                    self.bindings.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                for (param, ty) in f.params.params.iter().zip([first, second]) {
                    if let Some(name) = &param.name {
                        names.push((name.as_ref().into(), ty.clone()));
                    }
                }
                let bindings = extend_bindings(&base, names);
                self.inferencer.infer(&f.def.body, &bindings, true)
            }
            _ => None,
        }
    }

    /// The type of a member of a value of type `target` (calling it if it's a nullary method)
    pub fn member(&mut self, target: &Type, name: &str) -> Option<Type> {
        self.inferencer.member_lookup(target, name, true, None, None)
    }

    /// The type of calling a function of the given type with arguments
    pub fn invoke(&mut self, function: &Type, args: &TypedArgs) -> Option<Type> {
        let bindings = self.bindings.clone();
        self.inferencer.invoke(function, args, None, &bindings)
    }
}
