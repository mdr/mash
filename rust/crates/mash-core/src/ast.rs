//! The lowered ("abstract") syntax tree that the evaluator and type inferencer work on. Port of
//! `AbstractSyntax.scala`.
//!
//! Trees are immutable and shared via `Rc`; desugaring rebuilds the parts it changes, keeping node ids. Each
//! node has a [`NodeId`] (for side tables such as inferred types) and the range of source text it came from.
//!
//! **Invariant:** the tree never contains runtime [`Value`](crate::value::Value)s. Closures hold `Rc<FnDef>`s
//! from this tree and the garbage collector does not look inside them, so constants are kept as plain Rust
//! data here.

use std::fmt;
use std::rc::Rc;

use mash_syntax::TextRange;
use mash_syntax::doc_comment::DocComment;

pub type E = Rc<Expr>;

/// Identifies an expression node. Unique within one interpreter (across all compilation units), so side
/// tables can mix nodes from the current line with nodes from function bodies defined earlier.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NodeId(pub u32);

/// Hands out fresh [`NodeId`]s. One per interpreter.
#[derive(Debug, Default)]
pub struct NodeIdGen {
    next: u32,
}

impl NodeIdGen {
    pub fn fresh(&mut self) -> NodeId {
        let id = NodeId(self.next);
        self.next += 1;
        id
    }
}

/// A unit of source text that was compiled together (a REPL line, a script, an init file), for error
/// messages and `.source`.
#[derive(Debug, PartialEq, Eq)]
pub struct SourceUnit {
    pub name: String,
    pub text: String,
}

impl SourceUnit {
    pub fn new(name: impl Into<String>, text: impl Into<String>) -> Rc<SourceUnit> {
        Rc::new(SourceUnit {
            name: name.into(),
            text: text.into(),
        })
    }

    pub fn snippet(&self, range: TextRange) -> &str {
        &self.text[range]
    }
}

pub struct Expr {
    pub id: NodeId,
    /// The source text this node came from; None for nodes synthesised by desugaring.
    pub range: Option<TextRange>,
    pub kind: ExprKind,
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.kind, f)
    }
}

impl Expr {
    pub fn new(ids: &mut NodeIdGen, range: Option<TextRange>, kind: ExprKind) -> E {
        Rc::new(Expr {
            id: ids.fresh(),
            range,
            kind,
        })
    }

    /// A node with the same id and range but different contents (used when desugaring).
    pub fn with_kind(&self, kind: ExprKind) -> E {
        Rc::new(Expr {
            id: self.id,
            range: self.range,
            kind,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Or,
    And,
    Equals,
    NotEquals,
    GreaterThan,
    GreaterThanEquals,
    LessThan,
    LessThanEquals,
    Plus,
    Minus,
    Multiply,
    Divide,
}

impl BinOp {
    pub fn symbol(self) -> &'static str {
        match self {
            BinOp::Or => "or",
            BinOp::And => "and",
            BinOp::Equals => "==",
            BinOp::NotEquals => "!=",
            BinOp::GreaterThan => ">",
            BinOp::GreaterThanEquals => ">=",
            BinOp::LessThan => "<",
            BinOp::LessThanEquals => "<=",
            BinOp::Plus => "+",
            BinOp::Minus => "-",
            BinOp::Multiply => "*",
            BinOp::Divide => "/",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QuoteKind {
    /// `"…"`: the string is tagged as a path
    Double,
    Single,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RedirectOp {
    /// `< file`
    StandardInput,
    /// `> file`
    StandardOutput,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(f64),
    Bool(bool),
    Null,
}

#[derive(Debug)]
pub enum InterpolationPart {
    Text(Rc<str>),
    Expr(E),
}

#[derive(Debug)]
pub enum ObjectEntry {
    /// `field: value`. A plain identifier field is a name, not a variable reference.
    Full { field: E, value: E },
    /// `{ foo }`, meaning `{ foo: foo }`
    Shorthand { name: Rc<str>, range: Option<TextRange> },
}

#[derive(Debug)]
pub enum Arg {
    Positional(E),
    /// `-abc` (one entry per flag character)
    ShortFlags {
        flags: Vec<char>,
        range: Option<TextRange>,
    },
    /// `--name` or `--name=value`
    LongFlag {
        name: Rc<str>,
        value: Option<E>,
        range: Option<TextRange>,
    },
}

impl Arg {
    pub fn range(&self) -> Option<TextRange> {
        match self {
            Arg::Positional(e) => e.range,
            Arg::ShortFlags { range, .. } | Arg::LongFlag { range, .. } => *range,
        }
    }
}

/// `@name` or `@(name args…)` on a function, class, method or parameter
#[derive(Debug)]
pub struct Attribute {
    pub name: Rc<str>,
    pub args: Option<Vec<Arg>>,
    pub range: Option<TextRange>,
}

#[derive(Debug)]
pub enum Pattern {
    Ident(Rc<str>),
    Hole,
    Object(Vec<ObjectPatternEntry>),
    List(Vec<Pattern>),
}

#[derive(Debug)]
pub struct ObjectPatternEntry {
    pub field: Rc<str>,
    /// None for the shorthand `{ field }`, which binds `field`
    pub pattern: Option<Pattern>,
}

impl Pattern {
    pub fn bound_names(&self) -> Vec<Rc<str>> {
        let mut names = vec![];
        self.collect_bound_names(&mut names);
        names
    }

    fn collect_bound_names(&self, names: &mut Vec<Rc<str>>) {
        match self {
            Pattern::Ident(name) => names.push(name.clone()),
            Pattern::Hole => {}
            Pattern::Object(entries) => {
                for entry in entries {
                    match &entry.pattern {
                        Some(pattern) => pattern.collect_bound_names(names),
                        None => names.push(entry.field.clone()),
                    }
                }
            }
            Pattern::List(patterns) => patterns.iter().for_each(|p| p.collect_bound_names(names)),
        }
    }
}

#[derive(Debug)]
pub struct Param {
    pub attributes: Vec<Attribute>,
    /// The parameter's name; None if it's a destructuring pattern
    pub name: Option<Rc<str>>,
    pub variadic: bool,
    pub default: Option<E>,
    pub pattern: Option<Pattern>,
    pub range: Option<TextRange>,
}

impl Param {
    pub fn has_attribute(&self, name: &str) -> bool {
        self.attributes.iter().any(|a| &*a.name == name)
    }

    pub fn bound_names(&self) -> Vec<Rc<str>> {
        let mut names: Vec<Rc<str>> = self.name.iter().cloned().collect();
        for name in self.pattern.iter().flat_map(Pattern::bound_names) {
            if !names.contains(&name) {
                names.push(name);
            }
        }
        names
    }
}

/// A `def`, method or lambda.
#[derive(Debug)]
pub struct FnDef {
    /// None for lambdas
    pub name: Option<Rc<str>>,
    pub doc: Option<DocComment>,
    pub attributes: Vec<Attribute>,
    pub params: Vec<Param>,
    pub body: E,
    /// The namespace declared by the compilation unit (`namespace foo.bar`), if any
    pub namespace: Option<Rc<[Rc<str>]>>,
    pub unit: Rc<SourceUnit>,
    pub range: Option<TextRange>,
}

impl FnDef {
    pub fn has_attribute(&self, name: &str) -> bool {
        self.attributes.iter().any(|a| &*a.name == name)
    }

    /// The declaration's source text, if known
    pub fn source(&self) -> Option<&str> {
        self.range.map(|r| self.unit.snippet(r))
    }
}

#[derive(Debug)]
pub struct ClassDef {
    pub name: Rc<str>,
    pub doc: Option<DocComment>,
    pub attributes: Vec<Attribute>,
    /// Constructor parameters, which become the fields of instances
    pub params: Vec<Param>,
    pub methods: Vec<Rc<FnDef>>,
    pub namespace: Option<Rc<[Rc<str>]>>,
    pub unit: Rc<SourceUnit>,
    pub range: Option<TextRange>,
}

#[derive(Debug)]
pub struct MishRedirect {
    pub op: RedirectOp,
    pub arg: E,
}

#[derive(Debug)]
pub enum ExprKind {
    Literal(Literal),
    String {
        value: Rc<str>,
        quote: QuoteKind,
        tilde: bool,
    },
    /// A double-quoted string with `$x` / `${…}` interpolations. Text parts are already unescaped.
    Interpolated {
        parts: Vec<InterpolationPart>,
        tilde: bool,
    },
    Identifier(Rc<str>),
    /// `_` or `_1`… (removed by desugaring)
    Hole(u8),
    /// `.member` or `?.member` (removed by desugaring)
    HeadlessMember {
        name: Rc<str>,
        null_safe: bool,
    },
    This,
    /// `!ls`: a system command as a function
    MishFunction(Rc<str>),
    Paren(E),
    Block(E),
    StatementSeq(Vec<E>),
    List(Vec<E>),
    Object(Vec<ObjectEntry>),
    Member {
        target: E,
        name: Rc<str>,
        null_safe: bool,
        name_range: Option<TextRange>,
    },
    Lookup {
        target: E,
        index: E,
    },
    Invocation {
        function: E,
        args: Vec<Arg>,
        paren: bool,
    },
    Minus(E),
    BinOp {
        op: BinOp,
        lhs: E,
        rhs: E,
    },
    /// Chained comparisons such as `a < b <= c`
    ChainedOp {
        first: E,
        rest: Vec<(BinOp, E)>,
    },
    If {
        cond: E,
        then: E,
        else_: Option<E>,
    },
    /// `lhs = rhs`, or `lhs += rhs` etc.
    Assignment {
        lhs: E,
        op: Option<BinOp>,
        rhs: E,
    },
    PatternAssignment {
        pattern: Pattern,
        rhs: E,
    },
    Lambda(Rc<FnDef>),
    FunctionDecl(Rc<FnDef>),
    ClassDecl(Rc<ClassDef>),
    /// `a | b` (removed by desugaring)
    Pipe {
        lhs: E,
        rhs: E,
    },
    /// `!{cmd args…}` or `!!{…}`, or a line in mish mode
    Mish {
        command: E,
        args: Vec<E>,
        redirects: Vec<MishRedirect>,
        capture_output: bool,
    },
    /// An interpolation (`$x`, `${…}`) inside mish syntax
    MishInterpolation(E),
    /// `expr?`
    Help(E),
    /// `import foo.bar` or `import foo._` (name None)
    Import {
        expr: E,
        name: Option<Rc<str>>,
    },
    /// A part missing from an incomplete parse. Only produced by forgiving compilation (for completion);
    /// the evaluator reports it as an error if reached.
    Missing,
}

/// A compiled compilation unit.
#[derive(Debug)]
pub struct Program {
    pub namespace: Option<Rc<[Rc<str>]>>,
    pub body: E,
    pub unit: Rc<SourceUnit>,
}
