//! The `random` namespace. Randomness comes from the platform (deterministic in tests).

use mash_macros::builtin;

use crate::eval::{Ctx, R};
use crate::function::BuiltinFunction;

pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] = &[&IntFunction, &NumberFunction, &UuidFunction];

/// A random number in [0, 1)
fn random_f64(ctx: &Ctx) -> f64 {
    (ctx.platform().random_u64() >> 11) as f64 / (1u64 << 53) as f64
}

/// Generate a random integer in the given range
#[builtin(namespace = "random")]
pub fn int(
    ctx: &mut Ctx,
    #[param("Smallest number (inclusive)", default = 0i64)] from: i64,
    #[param("Largest number")] to: i64,
    #[param("If true, generate integers including the 'to' value", short = 'i', boolean_flag)] inclusive: bool,
) -> R<i64> {
    let range = (to - from + i64::from(inclusive)) as f64;
    Ok((random_f64(ctx) * range + from as f64) as i64)
}

/// A random number between 0 (inclusive) and 1 (exclusive)
#[builtin(namespace = "random")]
pub fn number(ctx: &mut Ctx) -> R<f64> {
    Ok(random_f64(ctx))
}

/// A random UUID
#[builtin(namespace = "random")]
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
