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
#[derive(Debug, Default, Clone)]
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
    /// Absorbs the last positional argument (the target of a pipe)
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
    /// Optional, with no value if not given (`Args::get_opt` gives None)
    NoArg,
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

    // Builder methods, for builtins

    pub fn summary(mut self, summary: &'static str) -> Self {
        self.summary = Some(summary.into());
        self
    }

    pub fn description(mut self, description: &'static str) -> Self {
        self.description = Some(description.into());
        self
    }

    pub fn short(mut self, c: char) -> Self {
        self.short_flag = Some(c);
        self
    }

    pub fn default_value(mut self, default: fn() -> Value) -> Self {
        self.default = Some(DefaultValue::Builtin(default));
        self
    }

    /// Optional, with no value when not given
    pub fn optional(mut self) -> Self {
        self.default = Some(DefaultValue::NoArg);
        self
    }

    /// A `--name` boolean flag, false unless given
    pub fn boolean_flag(mut self) -> Self {
        self.flag = true;
        self.boolean_flag = true;
        self.default = Some(DefaultValue::Builtin(|| Value::Bool(false)));
        self
    }

    /// Only given as a flag (`--name=value`)
    pub fn flag(mut self) -> Self {
        self.flag = true;
        self
    }

    pub fn flag_value_name(mut self, name: &'static str) -> Self {
        self.flag_value_name = Some(name.into());
        self
    }

    pub fn mandatory_flag_value(mut self) -> Self {
        self.flag_value_mandatory = true;
        self
    }

    pub fn variadic(mut self) -> Self {
        self.variadic = true;
        self
    }

    pub fn at_least_one(mut self) -> Self {
        self.variadic_at_least_one = true;
        self
    }

    pub fn flatten(mut self) -> Self {
        self.variadic_flatten = true;
        self
    }

    pub fn last(mut self) -> Self {
        self.last = true;
        self
    }

    pub fn lazy(mut self) -> Self {
        self.lazy = true;
        self
    }

    pub fn named_args(mut self) -> Self {
        self.named_args = true;
        self
    }

    pub fn all_args(mut self) -> Self {
        self.all_args = true;
        self
    }

    pub fn safe(mut self) -> Self {
        self.safe = true;
        self
    }
}

impl Params {
    pub fn new(params: Vec<Param>) -> Params {
        Params { params }
    }

    pub fn is_empty(&self) -> bool {
        self.params.is_empty()
    }

    /// Index of the parameter with this name
    pub fn index_of(&self, name: &str) -> Option<usize> {
        self.params.iter().position(|p| p.name.as_deref() == Some(name))
    }

    /// Parameters that arguments can be given to positionally
    pub fn positional(&self) -> impl Iterator<Item = &Param> {
        self.params.iter().filter(|p| !p.flag && !p.named_args)
    }

    /// Index of the parameter with this name, or this short flag. Later parameters win, as in Scala.
    pub fn index_by_flag(&self, name: &str) -> Option<usize> {
        let mut chars = name.chars();
        let short = match (chars.next(), chars.next()) {
            (Some(c), None) => Some(c),
            _ => None,
        };
        self.params
            .iter()
            .rposition(|p| p.name.as_deref() == Some(name) || (short.is_some() && p.short_flag == short))
    }

    /// Whether the function can be called with no arguments (and so is called automatically when referred
    /// to by name)
    pub fn allows_nullary(&self) -> bool {
        self.params.iter().all(Param::can_be_omitted)
    }

    pub fn allows_at_least_positional(&self, count: usize) -> bool {
        self.params.iter().any(|p| p.variadic || p.all_args) || self.positional().count() >= count
    }

    /// Names of parameters and short flags (for suggestions)
    pub fn flag_names(&self) -> Vec<String> {
        let mut names = vec![];
        for p in &self.params {
            names.extend(p.name.as_deref().map(str::to_string));
            names.extend(p.short_flag.map(|c| c.to_string()));
        }
        names
    }
}
