//! The `random` namespace. Randomness comes from the platform (deterministic in tests).

use mash_macros::builtin;

use crate::eval::{Ctx, MashError, R};
use crate::function::BuiltinFunction;

pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] = &[&IntFunction, &NumberFunction, &UuidFunction];

/// A random number in [0, 1)
fn random_f64(ctx: &Ctx) -> f64 {
    (ctx.platform().random_u64() >> 11) as f64 / (1u64 << 53) as f64
}

/// Generate a random integer in the given range
///
/// From `from` up to, but not including, `to` (with --inclusive, including it). An empty range is an error.
///
/// Examples:
/// <mash>
///   random.int 6              # 0 to 5
///   random.int 1 6 --inclusive # 1 to 6, as a die
/// </mash>
#[builtin(namespace = "random", returns = crate::infer::Type::NUMBER)]
pub fn int(
    ctx: &mut Ctx,
    #[param("Smallest number (inclusive)", default = 0i64)] from: i64,
    #[param("Number at the end of the range (exclusive, unless --inclusive)")] to: i64,
    #[param("If true, generate integers including the 'to' value", short = 'i', boolean_flag)] inclusive: bool,
) -> R<i64> {
    let count = i128::from(to) - i128::from(from) + i128::from(inclusive);
    if count <= 0 {
        let end = if inclusive { "up to and including" } else { "up to" };
        return Err(MashError::new(format!("No integers from {from} {end} {to}")));
    }
    let offset = uniform_below(ctx, count as u128);
    Ok((i128::from(from) + offset as i128) as i64)
}

/// A uniformly random integer in [0, count), for count up to 2^64: drawing again when a draw falls in the
/// incomplete last block, so that no value is more likely than another
fn uniform_below(ctx: &Ctx, count: u128) -> u128 {
    const DRAWS: u128 = 1 << 64;
    let usable = DRAWS - DRAWS % count;
    loop {
        let draw = u128::from(ctx.platform().random_u64());
        if draw < usable {
            return draw % count;
        }
    }
}

/// A random number between 0 (inclusive) and 1 (exclusive)
#[builtin(namespace = "random", returns = crate::infer::Type::NUMBER)]
pub fn number(ctx: &mut Ctx) -> R<f64> {
    Ok(random_f64(ctx))
}

/// A random UUID
#[builtin(namespace = "random", returns = crate::infer::Type::STRING)]
pub fn uuid(ctx: &mut Ctx) -> R<String> {
    let (high, low) = (ctx.platform().random_u64(), ctx.platform().random_u64());
    // Version 4 (random), variant 1
    let high = (high & !0xf000) | 0x4000;
    let low = (low & !(0b11 << 62)) | (0b10 << 62);
    let hex = format!("{high:016x}{low:016x}");
    Ok(format!(
        "{}-{}-{}-{}-{}",
        &hex[0..8],
        &hex[8..12],
        &hex[12..16],
        &hex[16..20],
        &hex[20..32]
    ))
}
