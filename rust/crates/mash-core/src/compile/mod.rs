//! From source text to the evaluator's AST: parse, lower, desugar (Scala `Compiler`).

mod bare_stringify;
mod desugar;
mod lower;

use std::collections::HashSet;
use std::rc::Rc;

use mash_syntax::{Mode, SyntaxError, TextRange};

use crate::ast::{NodeIdGen, Program, SourceUnit};

pub use bare_stringify::bare_stringify;
pub use desugar::{HOLE_VARIABLE_PREFIX, add_holes_to_headless_members, desugar_holes, desugar_pipes, remove_parens};
pub use lower::SourceMap;

#[derive(Debug, Clone, Copy, Default)]
pub struct Settings {
    /// Treat unbound identifiers as strings (`cd foo`)
    pub bare_words: bool,
    /// Parse the whole input as a line of mish (shell) syntax
    pub mish: bool,
}

#[derive(Debug)]
pub struct Compiled {
    pub program: Program,
    /// Source ranges to node ids (for IDE features)
    pub source_map: SourceMap,
    /// Identifiers that were turned into strings by bare-words mode
    pub bare_words: Vec<TextRange>,
}

/// Compile a unit of source text. Fails with the syntax errors if it doesn't parse. `bindings` are the
/// names already bound (used to decide what is a bare word).
pub fn compile(
    unit: Rc<SourceUnit>,
    settings: Settings,
    bindings: &HashSet<Rc<str>>,
    ids: &mut NodeIdGen,
) -> Result<Compiled, Vec<SyntaxError>> {
    let parse = parse(&unit, settings);
    if !parse.ok() {
        return Err(parse.errors().to_vec());
    }
    Ok(compile_parse(&parse, unit, settings, bindings, ids))
}

/// Compile whatever can be made of the input, even if it doesn't parse (for completion and other IDE
/// features). Missing parts become `ExprKind::Missing`.
pub fn compile_forgiving(
    unit: Rc<SourceUnit>,
    settings: Settings,
    bindings: &HashSet<Rc<str>>,
    ids: &mut NodeIdGen,
) -> Compiled {
    let parse = parse(&unit, settings);
    compile_parse(&parse, unit, settings, bindings, ids)
}

fn parse(unit: &SourceUnit, settings: Settings) -> mash_syntax::Parse {
    mash_syntax::parse(&unit.text, if settings.mish { Mode::Mish } else { Mode::Mash })
}

/// Lower without desugaring
pub fn lower(parse: &mash_syntax::Parse, unit: Rc<SourceUnit>, ids: &mut NodeIdGen) -> (Program, SourceMap) {
    let mut lowerer = lower::Lowerer::new(ids, unit);
    let program = lowerer.source_file(&parse.source_file());
    (program, lowerer.source_map)
}

fn compile_parse(
    parse: &mash_syntax::Parse,
    unit: Rc<SourceUnit>,
    settings: Settings,
    bindings: &HashSet<Rc<str>>,
    ids: &mut NodeIdGen,
) -> Compiled {
    let (program, source_map) = lower(parse, unit.clone(), ids);
    let body = add_holes_to_headless_members(&program.body, ids);
    let body = desugar_holes(&body, ids, &unit);
    let body = desugar_pipes(&body);
    let (body, bare_words) = if settings.bare_words {
        bare_stringify(&body, bindings)
    } else {
        (body, vec![])
    };
    Compiled {
        program: Program { body, ..program },
        source_map,
        bare_words,
    }
}
