//! Functions in `collections`.

use mash_macros::builtin;

use super::seq_like::{SequenceLike, sequence_like};
use crate::eval::{Ctx, R};
use crate::function::Args;
use crate::value::Value;

/// Find the length of a sequence
///
/// The length of an object is its number of fields.
///
/// Examples:
/// <mash>
///   length [1, 2, 3]      # 3
///   length []             # 0
///   length { a: 1, b: 2 } # 2
/// </mash>
#[builtin(namespace = "collections", returns = crate::infer::Type::NUMBER)]
pub fn length(
    ctx: &mut Ctx,
    args: &Args,
    #[param("Sequence to find the length of", description = "Can be a List, String or Object")] sequence: Value,
) -> R<usize> {
    let _ = sequence;
    sequence_length(ctx, args)
}

/// The length of the `sequence` argument: items of a list, characters of a string or fields of an object
pub(crate) fn sequence_length(ctx: &mut Ctx, args: &Args) -> R<usize> {
    Ok(match sequence_like(ctx, args, "sequence")? {
        SequenceLike::List(xs) => xs.len(),
        SequenceLike::Str(s) => s.len(),
        SequenceLike::Object(obj) => obj.len(),
    })
}
