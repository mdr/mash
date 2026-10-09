//! Static types of Mash expressions (Scala `inference.Type`).

use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use indexmap::IndexMap;
use mash_syntax::doc_comment::DocComment;

use crate::ast::{ClassDef, FnDef};
use crate::builtins::core::*;
use crate::builtins::os::PATH_CLASS;
use crate::function::{BuiltinFunction, BuiltinMethod, Params};
use crate::value::{BuiltinClass, Scope};

/// The type of a value, as far as it can be inferred statically.
#[derive(Clone)]
pub enum Type {
    /// An instance of a builtin class
    Instance(&'static BuiltinClass),
    /// A string or number tagged with a class (e.g. a path)
    Tagged {
        base: &'static BuiltinClass,
        tag: &'static BuiltinClass,
    },
    /// A list of elements of the given type
    Seq(Rc<Type>),
    /// A builtin class with type parameters (e.g. `Group` with key and element types)
    Generic(&'static BuiltinClass, Rc<[Type]>),
    /// An object without a class, with some known fields
    Object(Rc<IndexMap<Rc<str>, Type>>),
    /// A user-defined class itself
    UserClass(Rc<UserClassType>),
    /// An instance of a user-defined class
    UserClassInstance(Rc<UserClassType>),
    /// A user-defined function or lambda
    UserFunction(Rc<UserFunctionType>),
    /// A method of a user-defined class bound to a target
    BoundUserMethod(Rc<Type>, Rc<UserFunctionType>),
    /// A builtin function (or something that behaves like one)
    BuiltinFunction(BuiltinFunctionType),
    /// A builtin method bound to a target
    BoundBuiltinMethod(Rc<Type>, &'static dyn BuiltinMethod),
}

/// The variables in scope at some point, and their types.
pub type Bindings = Rc<HashMap<Rc<str>, Type>>;

/// Bindings extended with more names
pub fn extend_bindings(bindings: &Bindings, more: impl IntoIterator<Item = (Rc<str>, Type)>) -> Bindings {
    let mut more = more.into_iter().peekable();
    if more.peek().is_none() {
        return bindings.clone();
    }
    let mut map = (**bindings).clone();
    map.extend(more);
    Rc::new(map)
}

pub struct UserFunctionType {
    pub name: Option<Rc<str>>,
    pub doc: Option<DocComment>,
    pub private: bool,
    pub params: Rc<Params>,
    pub def: Rc<FnDef>,
    /// What's in scope where the function was defined
    pub bindings: FunctionBindings,
}

/// The variables in scope where a function was defined: known types (for functions found in code being
/// inferred), or a runtime scope whose variables' types are worked out when needed (for function values).
#[derive(Clone)]
pub enum FunctionBindings {
    Types(Bindings),
    /// A scope, plus extra bindings that take precedence (e.g. a class's other methods)
    Scope(Scope, Bindings),
}

pub struct UserClassType {
    pub name: Rc<str>,
    pub params: Rc<Params>,
    pub def: Option<Rc<ClassDef>>,
    /// Methods by name and alias, in order
    pub methods: Vec<(Rc<str>, Rc<UserFunctionType>)>,
}

impl UserClassType {
    pub fn method(&self, name: &str) -> Option<&Rc<UserFunctionType>> {
        self.methods.iter().find(|(n, _)| &**n == name).map(|(_, m)| m)
    }
}

/// Things typed as builtin functions
#[derive(Clone)]
pub enum BuiltinFunctionType {
    Builtin(&'static dyn BuiltinFunction),
    /// A user-defined class's constructor (`Point.new`)
    Constructor(Rc<UserClassType>),
    /// `!ls`
    SystemCommand,
}

impl Type {
    pub const ANY: Type = Type::Instance(&ANY_CLASS);
    pub const NUMBER: Type = Type::Instance(&NUMBER_CLASS);
    pub const STRING: Type = Type::Instance(&STRING_CLASS);
    pub const BOOLEAN: Type = Type::Instance(&BOOLEAN_CLASS);
    pub const NULL: Type = Type::Instance(&NULL_CLASS);
    pub const UNIT: Type = Type::Instance(&UNIT_CLASS);
    pub const OBJECT: Type = Type::Instance(&OBJECT_CLASS);
    pub const CLASS: Type = Type::Instance(&CLASS_CLASS);
    pub const FUNCTION: Type = Type::Instance(&FUNCTION_CLASS);
    pub const PATH: Type = Type::Tagged {
        base: &STRING_CLASS,
        tag: &PATH_CLASS,
    };
    pub const CHARACTER: Type = Type::Tagged {
        base: &STRING_CLASS,
        tag: &CHARACTER_CLASS,
    };

    pub fn seq(element: Type) -> Type {
        Type::Seq(Rc::new(element))
    }

    /// A string tagged with `tag`
    pub fn tagged_string(tag: &'static BuiltinClass) -> Type {
        Type::Tagged {
            base: &STRING_CLASS,
            tag,
        }
    }

    /// A number tagged with `tag`
    pub fn tagged_number(tag: &'static BuiltinClass) -> Type {
        Type::Tagged {
            base: &NUMBER_CLASS,
            tag,
        }
    }

    pub fn object(fields: impl IntoIterator<Item = (Rc<str>, Type)>) -> Type {
        Type::Object(Rc::new(fields.into_iter().collect()))
    }

    pub fn generic(class: &'static BuiltinClass, types: impl IntoIterator<Item = Type>) -> Type {
        Type::Generic(class, types.into_iter().collect())
    }

    pub fn is_any(&self) -> bool {
        matches!(self, Type::Instance(c) if std::ptr::eq(*c, &ANY_CLASS))
    }

    /// Whether this is a string, tagged or not
    pub fn is_string(&self) -> bool {
        match self {
            Type::Instance(c) | Type::Tagged { base: c, .. } => std::ptr::eq(*c, &STRING_CLASS),
            _ => false,
        }
    }

    /// Whether this is a number, tagged or not
    pub fn is_number(&self) -> bool {
        match self {
            Type::Instance(c) | Type::Tagged { base: c, .. } => std::ptr::eq(*c, &NUMBER_CLASS),
            _ => false,
        }
    }

    pub fn is_instance_of(&self, class: &'static BuiltinClass) -> bool {
        matches!(self, Type::Instance(c) if std::ptr::eq(*c, class))
    }

    /// The element type of a sequence type
    pub fn element(&self) -> Option<&Type> {
        match self {
            Type::Seq(element) => Some(element),
            _ => None,
        }
    }

    /// The type of the elements when this is treated as a sequence: lists by element, strings as characters
    pub fn sequence_element(&self) -> Option<Type> {
        match self {
            Type::Seq(element) => Some((**element).clone()),
            t if t.is_string() => Some(match t {
                Type::Tagged { tag, .. } => Type::tagged_string(tag),
                _ => Type::CHARACTER,
            }),
            _ => None,
        }
    }
}

impl From<&'static BuiltinClass> for Type {
    fn from(class: &'static BuiltinClass) -> Self {
        Type::Instance(class)
    }
}

fn ptr_eq<T: ?Sized>(a: &T, b: &T) -> bool {
    std::ptr::addr_eq(a as *const T, b as *const T)
}

/// Structural equality for builtin, collection and object types; identity for user-defined functions and
/// classes.
impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        use Type::*;
        match (self, other) {
            (Instance(a), Instance(b)) => ptr_eq(*a, *b),
            (Tagged { base: b1, tag: t1 }, Tagged { base: b2, tag: t2 }) => ptr_eq(*b1, *b2) && ptr_eq(*t1, *t2),
            (Seq(a), Seq(b)) => a == b,
            (Generic(c1, t1), Generic(c2, t2)) => ptr_eq(*c1, *c2) && t1 == t2,
            (Object(a), Object(b)) => a.len() == b.len() && a.iter().all(|(k, v)| b.get(k) == Some(v)),
            (UserClass(a), UserClass(b)) | (UserClassInstance(a), UserClassInstance(b)) => Rc::ptr_eq(a, b),
            (UserFunction(a), UserFunction(b)) => Rc::ptr_eq(a, b),
            (BoundUserMethod(t1, m1), BoundUserMethod(t2, m2)) => t1 == t2 && Rc::ptr_eq(m1, m2),
            (BuiltinFunction(a), BuiltinFunction(b)) => match (a, b) {
                (BuiltinFunctionType::Builtin(f), BuiltinFunctionType::Builtin(g)) => ptr_eq(*f, *g),
                (BuiltinFunctionType::Constructor(c), BuiltinFunctionType::Constructor(d)) => Rc::ptr_eq(c, d),
                (BuiltinFunctionType::SystemCommand, BuiltinFunctionType::SystemCommand) => true,
                _ => false,
            },
            (BoundBuiltinMethod(t1, m1), BoundBuiltinMethod(t2, m2)) => t1 == t2 && ptr_eq(*m1, *m2),
            _ => false,
        }
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Instance(c) => f.write_str(&c.full_name()),
            Type::Tagged { base, tag } => write!(f, "{} tagged {}", base.full_name(), tag.full_name()),
            Type::Seq(t) => write!(f, "[{t:?}]"),
            Type::Generic(c, ts) => write!(f, "{}{ts:?}", c.full_name()),
            Type::Object(fields) => {
                f.write_str("{")?;
                for (i, (k, v)) in fields.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{k}: {v:?}")?;
                }
                f.write_str("}")
            }
            Type::UserClass(c) => write!(f, "UserClass({})", c.name),
            Type::UserClassInstance(c) => write!(f, "UserClassInstance({})", c.name),
            Type::UserFunction(func) => write!(f, "UserFunction({})", func.name.as_deref().unwrap_or("anonymous")),
            Type::BoundUserMethod(t, m) => write!(f, "BoundUserMethod({t:?}, {})", m.name.as_deref().unwrap_or("?")),
            Type::BuiltinFunction(BuiltinFunctionType::Builtin(b)) => write!(f, "BuiltinFunction({})", b.full_name()),
            Type::BuiltinFunction(BuiltinFunctionType::Constructor(c)) => write!(f, "Constructor({})", c.name),
            Type::BuiltinFunction(BuiltinFunctionType::SystemCommand) => f.write_str("SystemCommand"),
            Type::BoundBuiltinMethod(t, m) => write!(f, "BoundBuiltinMethod({t:?}, {})", m.name()),
        }
    }
}

impl fmt::Debug for FunctionBindings {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionBindings::Types(b) => write!(f, "Types({} names)", b.len()),
            FunctionBindings::Scope(..) => f.write_str("Scope"),
        }
    }
}

impl fmt::Debug for UserFunctionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "UserFunctionType({:?})", self.name)
    }
}

impl fmt::Debug for UserClassType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "UserClassType({})", self.name)
    }
}

impl fmt::Debug for BuiltinFunctionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&Type::BuiltinFunction(self.clone()), f)
    }
}
