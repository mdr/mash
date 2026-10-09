use std::borrow::Cow;

use crate::value::Value;

/// The parameters of a function or method (Scala `ParameterModel`). Builtins build theirs once; user-defined
/// functions get theirs when the definition is evaluated (attributes such as `@(shortFlag 'x')` are
/// evaluated then).
#[derive(Debug, Default)]
pub struct Params {
    pub params: Vec<Param>,
}

/// One parameter (Scala `Parameter`).
#[derive(Debug, Default)]
pub struct Param {
    /// None for a parameter that destructures its argument with a pattern
    pub name: Option<Cow<'static, str>>,
    /// One-line description
    pub summary: Option<Cow<'static, str>>,
    /// Further documentation
    pub description: Option<Cow<'static, str>>,
    /// Single-character version of the flag
    pub short_flag: Option<char>,
    pub default: Option<DefaultValue>,
    /// Can be bound to zero or more arguments
    pub variadic: bool,
    /// As a variadic parameter, needs at least one argument
    pub variadic_at_least_one: bool,
    /// Variadic arguments that are lists are flattened
    pub variadic_flatten: bool,
    /// Can only be given as a flag (`--name=value`), not positionally
    pub flag: bool,
    /// The flag represents a boolean (only affects generated calling syntax)
    pub boolean_flag: bool,
    /// The flag needs a value (only affects generated calling syntax)
    pub flag_value_mandatory: bool,
    /// Name of the flag's value, for generated calling syntax
    pub flag_value_name: Option<Cow<'static, str>>,
    /// Absorbs the last positional argument
    pub last: bool,
    /// The argument isn't evaluated; the parameter is bound to a function that evaluates it
    pub lazy: bool,
    /// Receives an object of the named (flag) arguments
    pub named_args: bool,
    /// Receives all the arguments
    pub all_args: bool,
    /// Nullary functions passed to this parameter aren't invoked automatically
    pub safe: bool,
}

/// Where a parameter's default value comes from.
#[derive(Debug, Clone, Copy)]
pub enum DefaultValue {
    /// Computed by a builtin
    Builtin(fn() -> Value),
    /// The default expression of the corresponding parameter in a user-defined function, evaluated in the
    /// function's scope
    Expr,
}

impl Param {
    pub const ANONYMOUS_NAME: &'static str = "anonymousParam";

    pub fn named(name: impl Into<Cow<'static, str>>) -> Param {
        Param {
            name: Some(name.into()),
            ..Param::default()
        }
    }

    pub fn name(&self) -> &str {
        self.name.as_deref().unwrap_or(Self::ANONYMOUS_NAME)
    }

    pub fn has_default(&self) -> bool {
        self.default.is_some()
    }

    fn can_be_omitted(&self) -> bool {
        (self.variadic && !self.variadic_at_least_one) || self.has_default() || self.named_args || self.all_args
    }
}

impl Params {
    pub fn new(params: Vec<Param>) -> Params {
        Params { params }
    }

    pub fn is_empty(&self) -> bool {
        self.params.is_empty()
    }

    /// Parameters that arguments can be given to positionally
    pub fn positional(&self) -> impl Iterator<Item = &Param> {
        self.params.iter().filter(|p| !p.flag && !p.named_args)
    }

    /// The parameter with this name, or this short flag
    pub fn by_name(&self, name: &str) -> Option<&Param> {
        let mut chars = name.chars();
        let short = match (chars.next(), chars.next()) {
            (Some(c), None) => Some(c),
            _ => None,
        };
        // Later parameters win, as in Scala's map construction
        self.params
            .iter()
            .rev()
            .find(|p| p.name.as_deref() == Some(name) || (short.is_some() && p.short_flag == short))
    }

    /// Whether the function can be called with no arguments (and so is called automatically when referred
    /// to by name)
    pub fn allows_nullary(&self) -> bool {
        self.params.iter().all(Param::can_be_omitted)
    }

    pub fn allows_at_least_positional(&self, count: usize) -> bool {
        self.params.iter().any(|p| p.variadic || p.all_args) || self.positional().count() >= count
    }
}
