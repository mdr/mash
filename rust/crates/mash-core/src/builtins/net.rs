//! The `net` and `dns` namespaces: URLs, and looking up hosts. Lookups and opening URLs go through the
//! [`Platform`](crate::platform::Platform).

use mash_macros::{builtin, builtin_method};

use super::core::ANY_CLASS;
use super::os::paths::describe;
use crate::eval::{Ctx, MashError, R};
use crate::function::{Args, BuiltinFunction};
use crate::infer::Type;
use crate::value::{BuiltinClass, Object, Str, Value};

pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] = &[&UrlFunction, &UrlEncodeFunction, &LookupFunction];

pub static URL_CLASS: BuiltinClass = BuiltinClass {
    summary: "Tag class for a URL",
    methods: &[&HostMethod, &OpenMethod, &WithQueryParamsMethod],
    ..BuiltinClass::new("net", "Url", Some(&ANY_CLASS))
};

pub static HOST_CLASS: BuiltinClass = BuiltinClass {
    summary: "Host",
    methods: &[&IpAddressesMethod],
    ..BuiltinClass::new("net", "Host", Some(&ANY_CLASS))
};

/// Interpret the given value as a URL
#[builtin(namespace = "net", returns = crate::infer::Type::tagged_string(&URL_CLASS))]
pub fn url(ctx: &mut Ctx, args: &Args, #[param("String to interpret as a URL")] url: Value) -> R<Str> {
    let s = ctx.stringify(&url)?;
    if let Err(message) = check_uri(&s) {
        return Err(args.invalid("url", &message));
    }
    Ok(Str::tagged(s, &URL_CLASS))
}

/// URL encode the given value
#[builtin(namespace = "net", returns = crate::infer::Type::STRING)]
pub fn url_encode(ctx: &mut Ctx, #[param("String to URL encode")] string: Value) -> R<String> {
    let s = ctx.stringify(&string)?;
    Ok(form_encode(&s))
}

/// Return the host component of this URL (may be null)
#[builtin_method(returns = crate::infer::Type::tagged_string(&HOST_CLASS))]
fn host(target: Str) -> R<Value> {
    Ok(match uri_host(target.as_str()) {
        Some(host) => Value::Str(Str::tagged(host, &HOST_CLASS)),
        None => Value::Null,
    })
}

/// Add query parameters to this URL
///
/// Examples:
/// <mash>
///   url = net.url "http://example.com?existing=1"
///   url.withQueryParams { param: 42 } # http://example.com?existing=1&param=42
///   url.withQueryParams --param=42    # http://example.com?existing=1&param=42
/// </mash>
#[builtin_method(returns = crate::infer::Type::tagged_string(&URL_CLASS))]
fn with_query_params(
    ctx: &mut Ctx,
    target: Str,
    #[param("An Object containing query parameters to add to URL")] query_params: Option<Object>,
    #[param("Query parameters to add to URL", named_args)] named_query_params: Object,
) -> R<Str> {
    let mut params = vec![];
    for object in query_params.iter().chain([&named_query_params]) {
        for (field, value) in object.entries() {
            params.push((ctx.stringify(&field)?, ctx.stringify(&value)?));
        }
    }
    Ok(Str::tagged(add_query_params(target.as_str(), &params), &URL_CLASS))
}

/// Open a this URL with the default application
#[builtin_method(returns = Type::UNIT)]
fn open(ctx: &mut Ctx, target: &Value) -> R<()> {
    let url = ctx.stringify(target)?;
    ctx.platform()
        .open_with_default_application(&url)
        .map_err(|e| MashError::new(format!("Could not open '{url}': {}", describe(&e))))
}

/// The IP addresses of a host, as strings
fn ip_addresses_of(ctx: &Ctx, host: &str) -> R<Value> {
    let addresses = ctx
        .platform()
        .lookup_host(host)
        .map_err(|e| MashError::new(describe(&e)))?;
    Ok(Value::list(addresses.iter().map(|a| Value::str(a)).collect()))
}

/// Look up the IP addresses of the given host
#[builtin_method(returns = Type::seq(Type::STRING))]
fn ip_addresses(ctx: &mut Ctx, target: Str) -> R<Value> {
    ip_addresses_of(ctx, target.as_str())
}

/// Look up the IP addresses of the given host
#[builtin(namespace = "dns", returns = Type::seq(Type::STRING))]
pub fn lookup(ctx: &mut Ctx, #[param("Host name")] host: Str) -> R<Value> {
    ip_addresses_of(ctx, host.as_str())
}

/// `url` with `name=value` query parameters added (form-encoded), before any fragment (Apache's `URIBuilder`)
pub(crate) fn add_query_params(url: &str, params: &[(String, String)]) -> String {
    let params: Vec<String> = params
        .iter()
        .map(|(name, value)| format!("{}={}", form_encode(name), form_encode(value)))
        .collect();
    let (before_fragment, fragment) = match url.find('#') {
        Some(i) => url.split_at(i),
        None => (url, ""),
    };
    let mut result = before_fragment.to_string();
    if !params.is_empty() {
        let separator = match before_fragment.find('?') {
            Some(i) if i + 1 < before_fragment.len() => "&",
            Some(_) => "",
            None => "?",
        };
        result.push_str(separator);
        result.push_str(&params.join("&"));
    }
    result.push_str(fragment);
    result
}

/// `application/x-www-form-urlencoded` encoding, as Java's `URLEncoder`: letters, digits and `.-*_` are kept,
/// space becomes `+`, and anything else is percent-encoded as UTF-8
pub(crate) fn form_encode(s: &str) -> String {
    let mut out = String::new();
    for b in s.bytes() {
        match b {
            b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'.' | b'-' | b'*' | b'_' => out.push(b as char),
            b' ' => out.push('+'),
            b => out.push_str(&format!("%{b:02X}")),
        }
    }
    out
}

/// Check a URI reference for characters `java.net.URI` rejects, with a message like Java's
pub(crate) fn check_uri(s: &str) -> Result<(), String> {
    let chars: Vec<char> = s.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        let illegal = c.is_whitespace() || c.is_control() || "\"<>\\^`{|}".contains(c);
        if illegal {
            return Err(format!("Illegal character in {} at index {i}: {s}", component_at(s, i)));
        }
        if c == '%' {
            let hex = |j: usize| chars.get(j).is_some_and(char::is_ascii_hexdigit);
            if !hex(i + 1) || !hex(i + 2) {
                return Err(format!("Malformed escape pair at index {i}: {s}"));
            }
        }
        i += 1;
    }
    Ok(())
}

/// Which part of a URI the character at `index` (in chars) is in
fn component_at(s: &str, index: usize) -> &'static str {
    let before: String = s.chars().take(index).collect();
    if before.contains('#') {
        "fragment"
    } else if before.contains('?') {
        "query"
    } else if let Some(start) = before.find("://").map(|i| i + 3)
        && !before[start..].contains('/')
    {
        "authority"
    } else {
        "path"
    }
}

/// The host of a hierarchical URI with an authority (`scheme://[user@]host[:port]/…`), as `URI.getHost`
fn uri_host(s: &str) -> Option<String> {
    let scheme_end = s.find("://")?;
    let scheme = &s[..scheme_end];
    let mut scheme_chars = scheme.chars();
    if !scheme_chars.next()?.is_ascii_alphabetic()
        || !scheme_chars.all(|c| c.is_ascii_alphanumeric() || "+.-".contains(c))
    {
        return None;
    }
    let rest = &s[scheme_end + 3..];
    let authority = &rest[..rest.find(['/', '?', '#']).unwrap_or(rest.len())];
    let host_port = authority.rsplit_once('@').map_or(authority, |(_, h)| h);
    let host = if host_port.starts_with('[') {
        &host_port[..host_port.find(']').map_or(host_port.len(), |i| i + 1)]
    } else {
        host_port.split(':').next().unwrap_or_default()
    };
    (!host.is_empty()).then(|| host.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hosts() {
        assert_eq!(uri_host("http://example.com").as_deref(), Some("example.com"));
        assert_eq!(uri_host("http://u@[::1]:80/x").as_deref(), Some("[::1]"));
        assert_eq!(uri_host("https://a.b:8080/p?q#f").as_deref(), Some("a.b"));
        assert_eq!(uri_host("foo"), None);
        assert_eq!(uri_host("file:///tmp"), None);
    }

    #[test]
    fn encoding() {
        assert_eq!(form_encode("a b~é*"), "a+b%7E%C3%A9*");
        assert_eq!(form_encode("user@example.com"), "user%40example.com");
        assert_eq!(
            check_uri("a b"),
            Err("Illegal character in path at index 1: a b".into())
        );
        assert!(check_uri("http://example.com/a%20b?x=1#y").is_ok());
    }
}
