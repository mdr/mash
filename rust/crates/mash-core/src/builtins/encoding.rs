//! Small text-encoding namespaces: `html` (escaping), `base64` and `hash`.

use base64::Engine;
use base64::engine::general_purpose::STANDARD;
use mash_macros::builtin;
use sha2::{Digest, Sha256};

use super::os::PATH_CLASS;
use crate::eval::{Ctx, MashError, R};
use crate::function::BuiltinFunction;
use crate::value::{Str, Value};

pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] =
    &[&EscapeFunction, &EncodeFunction, &DecodeFunction, &Sha256Function];

/// Escape the characters using HTML entities
///
/// Escapes `&`, `<`, `>` and `"`. (Scala Mash also replaced non-ASCII characters that have named HTML 4
/// entities, such as `é` with `&eacute;`; with UTF-8 pages that isn't needed.)
#[builtin(namespace = "html", returns = crate::infer::Type::STRING)]
pub fn escape(#[param("String to escape")] string: Str) -> R<String> {
    let mut out = String::with_capacity(string.as_str().len());
    for c in string.as_str().chars() {
        match c {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            c => out.push(c),
        }
    }
    Ok(out)
}

/// Base 64 encode the given value
#[builtin(namespace = "base64", returns = crate::infer::Type::STRING)]
pub fn encode(ctx: &mut Ctx, #[param("String to encode in base 64")] string: Value) -> R<String> {
    let s = ctx.stringify(&string)?;
    Ok(STANDARD.encode(s.as_bytes()))
}

/// Base 64 decode the given value
#[builtin(namespace = "base64", returns = crate::infer::Type::STRING)]
pub fn decode(ctx: &mut Ctx, #[param("String to decode")] string: Value) -> R<String> {
    let s = ctx.stringify(&string)?;
    let bytes = STANDARD
        .decode(s.as_bytes())
        .map_err(|e| MashError::new(format!("Invalid base 64: {e}")))?;
    Ok(String::from_utf8_lossy(&bytes).into_owned())
}

/// Calculate the SHA-256 hash of the given input data
///
/// If a Path, then the contents of the file will be hashed. Else, the given data will be turned into a String
/// and hashed.
#[builtin(namespace = "hash", returns = crate::infer::Type::STRING)]
pub fn sha256(ctx: &mut Ctx, #[param("Data to hash")] data: Value) -> R<String> {
    if let Value::Str(s) = &data
        && s.tag().is_some_and(|tag| std::ptr::eq(tag, &PATH_CLASS))
    {
        return Err(MashError::new("Hashing the contents of files is not supported yet"));
    }
    let s = ctx.stringify(&data)?;
    let digest = Sha256::digest(s.as_bytes());
    Ok(digest.iter().map(|b| format!("{b:02x}")).collect())
}
