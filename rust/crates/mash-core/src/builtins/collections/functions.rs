//! Functions in `collections`.

use mash_macros::builtin;

use crate::eval::R;
use crate::function::Sequence;

/// Find the length of a sequence
///
/// Examples:
/// <mash>
///   length [1, 2, 3] # 3
///   length []        # 0
/// </mash>
#[builtin(namespace = "collections")]
pub fn length(#[param("Sequence to find the length of")] sequence: Sequence) -> R<usize> {
    Ok(match sequence {
        Sequence::List(xs) => xs.len(),
        Sequence::Str(s) => s.len(),
    })
}
