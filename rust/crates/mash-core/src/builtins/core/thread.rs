//! `core.thread`. The interpreter is single-threaded, so these run sequentially (an agreed change from
//! Scala, where they used threads).

use mash_macros::builtin;

use crate::eval::{Ctx, R};
use crate::function::{Callable, Sequence};
use crate::value::Value;

/// Fork off the given code in the background
///
/// (The code runs immediately, to completion; errors are ignored.)
#[builtin(namespace = "core.thread", returns = crate::infer::Type::UNIT)]
pub fn in_background(ctx: &mut Ctx, #[param("Code to execute", lazy)] block: Value) -> R<()> {
    match ctx.call_values(&block, vec![]) {
        Err(error) if error.is_interrupted() => Err(error),
        _ => Ok(()),
    }
}

/// Transform each element of a sequence by a given function
///
/// The given function is applied to each element of the input sequence
/// to produce a sequence of transformed output elements.
///
/// (The elements are processed one after another.)
///
/// Examples:
/// <mash>
///   parallelMap (_ * 2) [1, 2, 3] # [2, 4, 6]
/// </mash>
#[builtin(namespace = "core.thread", infer = super::infer_rules::parallel_map, completion = super::infer_rules::parallel_map_completion)]
pub fn parallel_map(
    ctx: &mut Ctx,
    #[param("Function used to transform elements of the sequence")] f: Callable,
    #[param("Sequence to map over")] sequence: Sequence,
) -> R<Value> {
    let mut results = vec![];
    for item in sequence.items() {
        results.push(f.call1(ctx, item)?);
    }
    Ok(sequence.rebuild(results))
}
