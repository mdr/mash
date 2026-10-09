//! Regular expressions (Scala `RegexClass`, `MatchClass`). A regex value is just a `Regex`-tagged string.
//!
//! Patterns use the `fancy-regex` syntax (Rust `regex` syntax plus lookaround and backreferences); case-insensitive
//! matching is Unicode-aware. Compiled patterns are cached.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use fancy_regex::{Regex, RegexBuilder};
use mash_macros::builtin_method;

use super::MATCH_CLASS;
use crate::eval::{MashError, R};
use crate::function::BuiltinMethod;
use crate::value::{ClassRef, Object, Str, Value};

pub(crate) const REGEX_METHODS: &[&dyn BuiltinMethod] = &[&MatchMethod];

/// Above this many cached patterns, the cache is emptied
const CACHE_LIMIT: usize = 256;

thread_local! {
    static CACHE: RefCell<HashMap<(String, bool), Rc<Regex>>> = RefCell::new(HashMap::new());
}

/// Compile a pattern (or fetch it from the cache)
pub(crate) fn compile(pattern: &str, ignore_case: bool) -> R<Rc<Regex>> {
    let key = (pattern.to_string(), ignore_case);
    if let Some(regex) = CACHE.with(|cache| cache.borrow().get(&key).cloned()) {
        return Ok(regex);
    }
    let regex = RegexBuilder::new(pattern)
        .case_insensitive(ignore_case)
        .build()
        .map_err(|e| MashError::new(format!("Invalid regular expression '{pattern}': {e}")))?;
    let regex = Rc::new(regex);
    CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        if cache.len() >= CACHE_LIMIT {
            cache.clear();
        }
        cache.insert(key, regex.clone());
    });
    Ok(regex)
}

/// An error while matching (e.g. the backtracking limit was exceeded)
pub(crate) fn runtime_error(e: fancy_regex::Error) -> MashError {
    MashError::new(format!("Regular expression error: {e}"))
}

/// Find the first match of the regex in the given string, or else null
#[builtin_method(name = "match")]
fn match_(target: Str, #[param("String to search within for matches")] string: Str) -> R<Value> {
    let regex = compile(target.as_str(), false)?;
    let Some(captures) = regex.captures(string.as_str()).map_err(runtime_error)? else {
        return Ok(Value::Null);
    };
    let matched = captures.get(0).map_or("", |m| m.as_str());
    let groups: Vec<Value> = (1..captures.len())
        .map(|i| captures.get(i).map_or(Value::Null, |m| Value::str(m.as_str())))
        .collect();
    let object = Object::with_class(Some(ClassRef::Builtin(&MATCH_CLASS)));
    object.set_str("matched", Value::str(matched));
    object.set_str("groups", Value::list(groups));
    Ok(Value::Object(object))
}
