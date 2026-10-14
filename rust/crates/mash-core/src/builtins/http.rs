//! The `http` namespace: HTTP requests through the [`Platform`](crate::platform::Platform) (Scala
//! `HttpFunctions`, `HttpOperations`, `ResponseClass`, …), and the request machinery shared with `json.get` and
//! friends.
//!
//! TLS certificates are verified unless `--insecure` is given (Scala Mash trusted every certificate).

use mash_macros::{builtin, builtin_method};

use super::constructor::FieldsConstructor;
use super::core::OBJECT_CLASS;
use super::json::{json_string, parse_json};
use super::net::{add_query_params, check_uri, form_encode};
use super::os::paths::{describe, fs, io_error, path_of};
use crate::eval::{Ctx, MashError, R};
use crate::function::{Args, BuiltinFunction, Param};
use crate::infer::Type;
use crate::platform::{HttpRequest, HttpResponse};
use crate::value::{BuiltinClass, ClassRef, Field, Object, Str, Value};

pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] = &[
    &GetFunction,
    &PostFunction,
    &PutFunction,
    &PatchFunction,
    &DeleteFunction,
];

pub(crate) const CLASSES: &[&BuiltinClass] = &[&RESPONSE_CLASS, &HEADER_CLASS, &COOKIE_CLASS];

pub static RESPONSE_CLASS: BuiltinClass = BuiltinClass {
    summary: "An HTTP response",
    fields: &[
        Field {
            name: "status",
            summary: "Status code",
            ty: || Type::NUMBER,
        },
        Field {
            name: "body",
            summary: "Body of the response",
            ty: || Type::STRING,
        },
        Field {
            name: "headers",
            summary: "Response headers",
            ty: || Type::seq(Type::Instance(&HEADER_CLASS)),
        },
        Field {
            name: "cookies",
            summary: "Response cookies",
            ty: || Type::seq(Type::Instance(&COOKIE_CLASS)),
        },
    ],
    methods: &[
        &GetHeaderMethod,
        &GetHeadersMethod,
        &HeadersObjectMethod,
        &JsonMethod,
        &SucceededMethod,
    ],
    static_methods: &[&RESPONSE_NEW],
    ..BuiltinClass::new("http", "Response", Some(&OBJECT_CLASS))
};

pub static HEADER_CLASS: BuiltinClass = BuiltinClass {
    summary: "An HTTP header",
    fields: &[
        Field {
            name: "name",
            summary: "Header field name",
            ty: || Type::STRING,
        },
        Field {
            name: "value",
            summary: "Header field value",
            ty: || Type::STRING,
        },
    ],
    static_methods: &[&HEADER_NEW],
    ..BuiltinClass::new("http", "Header", Some(&OBJECT_CLASS))
};

pub static COOKIE_CLASS: BuiltinClass = BuiltinClass {
    summary: "An HTTP cookie",
    fields: &[
        Field {
            name: "name",
            summary: "Cookie name",
            ty: || Type::STRING,
        },
        Field {
            name: "value",
            summary: "Cookie value",
            ty: || Type::STRING,
        },
    ],
    static_methods: &[&COOKIE_NEW],
    ..BuiltinClass::new("http", "Cookie", Some(&OBJECT_CLASS))
};

static RESPONSE_NEW: FieldsConstructor =
    FieldsConstructor::new("http.Response", &RESPONSE_CLASS, "Construct a new Response object");
static HEADER_NEW: FieldsConstructor =
    FieldsConstructor::new("http.Header", &HEADER_CLASS, "Construct a new Header object");
static COOKIE_NEW: FieldsConstructor =
    FieldsConstructor::new("http.Cookie", &COOKIE_CLASS, "Construct a new Cookie object");

fn response_type() -> Type {
    Type::Instance(&RESPONSE_CLASS)
}

// ------------------------------------------------------------------------------------------------ parameters

pub(crate) fn url_param() -> Param {
    Param::named("url").summary("URL to send request to")
}

pub(crate) fn body_param() -> Param {
    Param::named("body")
        .summary("Body of request")
        .default_value(|| Value::str(""))
}

pub(crate) fn file_param() -> Param {
    Param::named("file")
        .summary("Whether the body should be fetched from a file (default false)")
        .description(
            "If true, the body parameter will be interpreted as a path, and the contents of the file at that location used as the body.\nIf false, the body will be used directly.",
        )
        .short('f')
        .boolean_flag()
}

pub(crate) fn basic_auth_param() -> Param {
    Param::named("basicAuth")
        .summary("Basic authentication")
        .description(
            "Must provide the username and password either as a colon-separated String:\n<mash>\n  --basicAuth='username:password'\n</mash>\nor an object of the form:\n<mash>\n  --basicAuth={ username: 'username', password: 'password' }\n</mash>",
        )
        .flag()
}

pub(crate) fn headers_param() -> Param {
    Param::named("headers")
        .summary("Headers to add to request")
        .description(
            "Headers can be provided either as an object or a list of name/value pairs:\n<mash>\n  --headers={ header1: value }\n  --headers=[\"header1:value\", \"header2:value\"]\n  --headers=[{ name: \"header1\", value: \"value\" }]\n</mash>",
        )
        .flag()
}

pub(crate) fn cookies_param() -> Param {
    Param::named("cookies")
        .summary("Cookies to add to the request")
        .description("Cookies can be provided as an object:\n<mash>\n  --cookies={ sessionId: 'xyz' }\n</mash>")
        .flag()
}

pub(crate) fn query_params_param() -> Param {
    Param::named("queryParams")
        .summary("Query parameters to add to the URL")
        .description("Examples:\n<mash>\n  --queryParams={ name: \"value\" }\n</mash>")
        .flag()
        .default_value(|| Value::Object(Object::new()))
}

fn json_param() -> Param {
    Param::named("json")
        .summary("Whether to send the body as JSON (default false)")
        .short('j')
        .boolean_flag()
}

fn form_param() -> Param {
    Param::named("form")
        .summary("Whether to send the body as a URL-encoded form (default false)")
        .boolean_flag()
}

pub(crate) fn timeout_param() -> Param {
    Param::named("timeout")
        .summary("Give up if the request takes longer than this (default: wait as long as it takes)")
        .description(
            "A duration, such as 10.seconds, or a number of milliseconds (as sleep). It covers the whole request: \
connecting, sending, receiving the response, and following redirects.\nWith or without a timeout, Ctrl-C interrupts a request.",
        )
        .flag()
}

pub(crate) fn insecure_param() -> Param {
    Param::named("insecure")
        .summary("Don't verify the server's TLS certificate (default false)")
        .description(
            "By default, HTTPS requests fail if the server's certificate isn't trusted, or doesn't match its host name.",
        )
        .boolean_flag()
}

// ------------------------------------------------------------------------------------------------ requests

/// Where a request goes, and the options every request function has
pub(crate) struct Target {
    pub(crate) url: Str,
    pub(crate) basic_auth: Option<Value>,
    pub(crate) headers: Option<Value>,
    pub(crate) cookies: Option<Value>,
    pub(crate) query_params: Object,
    pub(crate) insecure: bool,
    /// `--timeout`, if given
    pub(crate) timeout: Option<Value>,
}

/// What a request carries as its body
pub(crate) struct Body {
    pub(crate) value: Value,
    /// `value` is the path of a file to send
    pub(crate) from_file: bool,
}

/// How to encode the body
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Encoding {
    /// As text (or a file's bytes), `application/octet-stream`
    Raw,
    /// As JSON (`application/json`); also asks for a JSON response (`Accept: application/json`)
    Json,
    /// As a URL-encoded form (the body must be an object)
    Form,
}

/// Make a request (Scala `HttpOperations.runRequest`)
pub(crate) fn send(
    ctx: &mut Ctx,
    args: &Args,
    method: &str,
    target: Target,
    body: Option<Body>,
    encoding: Encoding,
) -> R<HttpResponse> {
    let mut headers = match &target.headers {
        Some(value) => headers_of(ctx, args, value)?,
        None => vec![],
    };
    let cookies = match &target.cookies {
        Some(Value::Object(object)) => {
            let mut cookies = vec![];
            for (name, value) in object.entries() {
                cookies.push((ctx.stringify(&name)?, ctx.stringify(&value)?));
            }
            cookies
        }
        None | Some(Value::Null) => vec![],
        // Scala ignored anything else, silently sending no cookies
        Some(other) => {
            return Err(args.invalid(
                "cookies",
                &format!("Must be an object of cookies, but was a {}", other.type_name()),
            ));
        }
    };
    let basic_auth = match &target.basic_auth {
        Some(value) => Some(credentials(ctx, args, value)?),
        None => None,
    };
    let url = url_with_query(ctx, &target)?;
    let has_header =
        |headers: &[(String, String)], wanted: &str| headers.iter().any(|(name, _)| name.eq_ignore_ascii_case(wanted));
    if encoding == Encoding::Json && !has_header(&headers, "accept") {
        headers.insert(0, ("Accept".into(), "application/json".into()));
    }
    let body = match body {
        Some(body) => {
            let (bytes, content_type) = encode_body(ctx, args, body, encoding)?;
            if !has_header(&headers, "content-type") {
                headers.push(("Content-Type".into(), content_type.into()));
            }
            Some(bytes)
        }
        None => None,
    };
    let timeout = match &target.timeout {
        Some(value) => Some(crate::builtins::time::duration_of(ctx, value)?.ok_or_else(|| {
            args.invalid("timeout", &format!("Must be a duration, but was a {}", value.type_name()))
        })?),
        None => None,
    };
    let interrupt = ctx.interrupt_flag();
    let request = HttpRequest {
        method,
        url: &url,
        headers: &headers,
        body: body.as_deref(),
        basic_auth: basic_auth.as_ref().map(|(u, p)| (u.as_str(), p.as_str())),
        cookies: &cookies,
        insecure: target.insecure,
        timeout,
        interrupt: Some(&interrupt),
    };
    ctx.platform().http_request(&request).map_err(|e| {
        if e.kind() == std::io::ErrorKind::Interrupted && ctx.poll_interrupt() {
            MashError::interrupted()
        } else {
            MashError::new(describe(&e))
        }
    })
}

/// The URL with the query parameters added (Scala `QueryParameters.getUrl`); an error unless it's an absolute
/// `http` or `https` URL
fn url_with_query(ctx: &mut Ctx, target: &Target) -> R<String> {
    let url = target.url.as_str();
    check_uri(url).map_err(|message| MashError::new(format!("Invalid URL: {message}")))?;
    let lower = url.to_ascii_lowercase();
    if !lower.starts_with("http://") && !lower.starts_with("https://") {
        return Err(MashError::new(format!(
            "Invalid URL '{url}': must be an absolute http or https URL"
        )));
    }
    let mut params = vec![];
    for (field, value) in target.query_params.entries() {
        let name = ctx.stringify(&field)?;
        for value in values_of(&value) {
            params.push((name.clone(), ctx.stringify(&value)?));
        }
    }
    Ok(add_query_params(url, &params))
}

/// A list's items, or the value alone
fn values_of(value: &Value) -> Vec<Value> {
    match value {
        Value::List(xs) => xs.snapshot(),
        other => vec![other.clone()],
    }
}

/// A field of an object argument, as a string
fn string_field(ctx: &mut Ctx, args: &Args, param: &str, what: &str, object: &Object, field: &str) -> R<String> {
    match object.get_str(field) {
        Some(value) => ctx.stringify(&value),
        None => Err(args.invalid(param, &format!("{what} object must have '{field}' field"))),
    }
}

/// A `"name: value"` header: split at the first colon, as the value may contain more (a URL, a time)
fn header_string(args: &Args, s: &str) -> R<(String, String)> {
    match s.split_once(':') {
        Some((name, value)) if !name.trim().is_empty() => Ok((name.trim().to_string(), value.trim().to_string())),
        _ => Err(args.invalid("headers", "Header string must be 'name:value'")),
    }
}

/// The headers given as an object (a field per header, with a list for several values), a list of
/// `"name:value"` strings or `{ name, value }` objects (Scala `Header.getHeaders`), or one `"name:value"`
/// string. Anything else is an error (Scala ignored it, sending no headers).
fn headers_of(ctx: &mut Ctx, args: &Args, value: &Value) -> R<Vec<(String, String)>> {
    let mut headers = vec![];
    match value {
        Value::Null => {}
        Value::Str(s) => headers.push(header_string(args, s.as_str())?),
        Value::List(items) => {
            for item in items.snapshot() {
                headers.push(match &item {
                    Value::Str(s) => header_string(args, s.as_str())?,
                    Value::Object(object) => (
                        string_field(ctx, args, "headers", "Header", object, "name")?,
                        string_field(ctx, args, "headers", "Header", object, "value")?,
                    ),
                    other => {
                        return Err(args.invalid("headers", &format!("Invalid header of type {}", other.type_name())));
                    }
                });
            }
        }
        Value::Object(object) => {
            for (name, value) in object.entries() {
                let name = ctx.stringify(&name)?;
                for value in values_of(&value) {
                    headers.push((name.clone(), ctx.stringify(&value)?));
                }
            }
        }
        other => {
            return Err(args.invalid(
                "headers",
                &format!(
                    "Must be an object, a list or a string of headers, but was a {}",
                    other.type_name()
                ),
            ));
        }
    }
    Ok(headers)
}

/// A username and password, as `"username:password"` or `{ username, password }` (Scala
/// `BasicCredentials.getBasicCredentials`)
fn credentials(ctx: &mut Ctx, args: &Args, value: &Value) -> R<(String, String)> {
    match value {
        // The username can't contain a colon, but the password can (RFC 7617)
        Value::Str(s) => match s.as_str().split_once(':') {
            Some((username, password)) => Ok((username.to_string(), password.to_string())),
            None => Err(args.invalid("basicAuth", "Auth string must be 'username:password'")),
        },
        Value::Object(object) => Ok((
            string_field(ctx, args, "basicAuth", "Auth", object, "username")?,
            string_field(ctx, args, "basicAuth", "Auth", object, "password")?,
        )),
        other => Err(args.invalid(
            "basicAuth",
            &format!("Invalid authorisation of type {}", other.type_name()),
        )),
    }
}

/// The bytes of a request body and their content type (Scala `HttpOperations.setBody`)
fn encode_body(ctx: &mut Ctx, args: &Args, body: Body, encoding: Encoding) -> R<(Vec<u8>, &'static str)> {
    if encoding == Encoding::Form {
        let Value::Object(object) = &body.value else {
            return Err(args.invalid("form", "Invalid body, must be an object"));
        };
        let mut pairs = vec![];
        for (name, value) in object.entries() {
            pairs.push(format!(
                "{}={}",
                form_encode(&ctx.stringify(&name)?),
                form_encode(&ctx.stringify(&value)?)
            ));
        }
        return Ok((pairs.join("&").into_bytes(), "application/x-www-form-urlencoded"));
    }
    let content_type = match encoding {
        Encoding::Json => "application/json; charset=UTF-8",
        _ => "application/octet-stream",
    };
    let bytes = if body.from_file {
        let path = path_of(&body.value).ok_or_else(|| {
            args.invalid(
                "body",
                &format!(
                    "Could not interpret value of type {} as a path.",
                    body.value.type_name()
                ),
            )
        })?;
        ctx.platform()
            .read_file(fs(&path))
            .map_err(|e| io_error("read", &path, &e))?
    } else if encoding == Encoding::Json {
        json_string(ctx, &body.value, false)?.into_bytes()
    } else {
        ctx.stringify(&body.value)?.into_bytes()
    };
    Ok((bytes, content_type))
}

/// The body of a response as text (lossily decoded as UTF-8)
pub(crate) fn body_text(response: &HttpResponse) -> String {
    String::from_utf8_lossy(&response.body).into_owned()
}

/// A `name`/`value` object of a class (`Header` or `Cookie`)
fn name_value(class: &'static BuiltinClass, name: &str, value: &str) -> Value {
    let object = Object::with_class(Some(ClassRef::Builtin(class)));
    object.set_str("name", Value::str(name));
    object.set_str("value", Value::str(value));
    Value::Object(object)
}

/// The `http.Response` object for a response
fn response_object(response: &HttpResponse) -> Object {
    let object = Object::with_class(Some(ClassRef::Builtin(&RESPONSE_CLASS)));
    object.set_str("status", Value::num(f64::from(response.status)));
    object.set_str("body", Value::str(&body_text(response)));
    let headers = response
        .headers
        .iter()
        .map(|(name, value)| name_value(&HEADER_CLASS, name, value))
        .collect();
    object.set_str("headers", Value::list(headers));
    let cookies = response
        .cookies
        .iter()
        .map(|(name, value)| name_value(&COOKIE_CLASS, name, value))
        .collect();
    object.set_str("cookies", Value::list(cookies));
    object
}

/// Make a request and return the `http.Response`
fn respond(
    ctx: &mut Ctx,
    args: &Args,
    method: &str,
    target: Target,
    body: Option<Body>,
    json: bool,
    form: bool,
) -> R<Object> {
    if json && form {
        return Err(args.invalid("json", "Incompatible arguments json and form"));
    }
    let encoding = match (json, form) {
        (true, _) => Encoding::Json,
        (_, true) => Encoding::Form,
        _ => Encoding::Raw,
    };
    let response = send(ctx, args, method, target, body, encoding)?;
    Ok(response_object(&response))
}

// ------------------------------------------------------------------------------------------------ functions

/// Make an HTTP GET request
#[builtin(namespace = "http", returns = response_type())]
#[allow(clippy::too_many_arguments)]
pub(crate) fn get(
    ctx: &mut Ctx,
    args: &Args,
    #[param(def = url_param)] url: Str,
    #[param(def = basic_auth_param)] basic_auth: Option<Value>,
    #[param(def = headers_param)] headers: Option<Value>,
    #[param(def = cookies_param)] cookies: Option<Value>,
    #[param(def = query_params_param)] query_params: Object,
    #[param(def = json_param)] json: bool,
    #[param(def = insecure_param)] insecure: bool,
    #[param(def = timeout_param)] timeout: Option<Value>,
) -> R<Object> {
    let target = Target {
        url,
        basic_auth,
        headers,
        cookies,
        query_params,
        insecure,
        timeout,
    };
    respond(ctx, args, "GET", target, None, json, false)
}

/// Make an HTTP POST request
#[builtin(namespace = "http", returns = response_type())]
#[allow(clippy::too_many_arguments)]
pub(crate) fn post(
    ctx: &mut Ctx,
    args: &Args,
    #[param(def = url_param)] url: Str,
    #[param(def = body_param)] body: Value,
    #[param(def = file_param)] file: bool,
    #[param(def = json_param)] json: bool,
    #[param(def = form_param)] form: bool,
    #[param(def = basic_auth_param)] basic_auth: Option<Value>,
    #[param(def = headers_param)] headers: Option<Value>,
    #[param(def = cookies_param)] cookies: Option<Value>,
    #[param(def = query_params_param)] query_params: Object,
    #[param(def = insecure_param)] insecure: bool,
    #[param(def = timeout_param)] timeout: Option<Value>,
) -> R<Object> {
    let target = Target {
        url,
        basic_auth,
        headers,
        cookies,
        query_params,
        insecure,
        timeout,
    };
    let body = Body {
        value: body,
        from_file: file,
    };
    respond(ctx, args, "POST", target, Some(body), json, form)
}

/// Make an HTTP PUT request
#[builtin(namespace = "http", returns = response_type())]
#[allow(clippy::too_many_arguments)]
pub(crate) fn put(
    ctx: &mut Ctx,
    args: &Args,
    #[param(def = url_param)] url: Str,
    #[param(def = body_param)] body: Value,
    #[param(def = file_param)] file: bool,
    #[param(def = json_param)] json: bool,
    #[param(def = basic_auth_param)] basic_auth: Option<Value>,
    #[param(def = headers_param)] headers: Option<Value>,
    #[param(def = cookies_param)] cookies: Option<Value>,
    #[param(def = query_params_param)] query_params: Object,
    #[param(def = insecure_param)] insecure: bool,
    #[param(def = timeout_param)] timeout: Option<Value>,
) -> R<Object> {
    let target = Target {
        url,
        basic_auth,
        headers,
        cookies,
        query_params,
        insecure,
        timeout,
    };
    let body = Body {
        value: body,
        from_file: file,
    };
    respond(ctx, args, "PUT", target, Some(body), json, false)
}

/// Make an HTTP PATCH request
#[builtin(namespace = "http", returns = response_type())]
#[allow(clippy::too_many_arguments)]
pub(crate) fn patch(
    ctx: &mut Ctx,
    args: &Args,
    #[param(def = url_param)] url: Str,
    #[param(def = body_param)] body: Value,
    #[param(def = file_param)] file: bool,
    #[param(def = json_param)] json: bool,
    #[param(def = form_param)] form: bool,
    #[param(def = basic_auth_param)] basic_auth: Option<Value>,
    #[param(def = headers_param)] headers: Option<Value>,
    #[param(def = cookies_param)] cookies: Option<Value>,
    #[param(def = query_params_param)] query_params: Object,
    #[param(def = insecure_param)] insecure: bool,
    #[param(def = timeout_param)] timeout: Option<Value>,
) -> R<Object> {
    let target = Target {
        url,
        basic_auth,
        headers,
        cookies,
        query_params,
        insecure,
        timeout,
    };
    let body = Body {
        value: body,
        from_file: file,
    };
    respond(ctx, args, "PATCH", target, Some(body), json, form)
}

/// Make an HTTP DELETE request
#[builtin(namespace = "http", returns = response_type())]
#[allow(clippy::too_many_arguments)]
pub(crate) fn delete(
    ctx: &mut Ctx,
    args: &Args,
    #[param(def = url_param)] url: Str,
    #[param(def = basic_auth_param)] basic_auth: Option<Value>,
    #[param(def = headers_param)] headers: Option<Value>,
    #[param(def = cookies_param)] cookies: Option<Value>,
    #[param(def = query_params_param)] query_params: Object,
    #[param(def = json_param)] json: bool,
    #[param(def = insecure_param)] insecure: bool,
    #[param(def = timeout_param)] timeout: Option<Value>,
) -> R<Object> {
    let target = Target {
        url,
        basic_auth,
        headers,
        cookies,
        query_params,
        insecure,
        timeout,
    };
    respond(ctx, args, "DELETE", target, None, json, false)
}

// ------------------------------------------------------------------------------------------------ Response methods

/// A field of a response object
fn response_field(target: &Value, field: &str) -> R<Value> {
    let missing = || MashError::new(format!("Response has no '{field}' field"));
    match target {
        Value::Object(object) => object.get_str(field).ok_or_else(missing),
        _ => Err(missing()),
    }
}

fn response_body(target: &Value) -> R<String> {
    match response_field(target, "body")? {
        Value::Str(s) => Ok(s.as_str().to_string()),
        other => Err(MashError::new(format!(
            "Response field 'body' must be a String, but was a {}",
            other.type_name()
        ))),
    }
}

/// The response's headers, as names and values (Scala `ResponseClass.Wrapper.headers`)
fn response_headers(target: &Value) -> R<Vec<(String, String)>> {
    let Value::List(headers) = response_field(target, "headers")? else {
        return Err(MashError::new("Response field 'headers' must be a List"));
    };
    let field = |header: &Value, name: &str| match header {
        Value::Object(o) => match o.get_str(name) {
            Some(Value::Str(s)) => Ok(s.as_str().to_string()),
            _ => Err(MashError::new(format!("Header has no String field '{name}'"))),
        },
        other => Err(MashError::new(format!(
            "A header must be an object, but was a {}",
            other.type_name()
        ))),
    };
    headers
        .snapshot()
        .iter()
        .map(|header| Ok((field(header, "name")?, field(header, "value")?)))
        .collect()
}

/// Parse response body as JSON
#[builtin_method(returns = Type::OBJECT)]
fn json(target: &Value) -> R<Value> {
    parse_json(&response_body(target)?)
}

/// True if the HTTP request succeeded (status code 2xx)
#[builtin_method(returns = Type::BOOLEAN)]
fn succeeded(target: &Value) -> R<bool> {
    match response_field(target, "status")? {
        Value::Number(n, _) => Ok((200.0..=299.0).contains(&n.as_f64())),
        other => Err(MashError::new(format!(
            "Response field 'status' must be a Number, but was a {}",
            other.type_name()
        ))),
    }
}

/// The headers as an object
#[builtin_method(returns = Type::OBJECT)]
fn headers_object(target: &Value) -> R<Object> {
    let object = Object::new();
    for (name, value) in response_headers(target)? {
        object.set_str(&name, Value::str(&value));
    }
    Ok(object)
}

/// Get all headers with the given name, ignoring case
#[builtin_method(returns = Type::seq(Type::STRING))]
fn get_headers(target: &Value, #[param("Header name to search for")] name: Str) -> R<Value> {
    let wanted = name.as_str().to_lowercase();
    Ok(Value::list(
        response_headers(target)?
            .into_iter()
            .filter(|(name, _)| name.to_lowercase() == wanted)
            .map(|(_, value)| Value::str(&value))
            .collect(),
    ))
}

/// Get header with the given name, ignoring case, else null
#[builtin_method(returns = Type::STRING)]
fn get_header(target: &Value, #[param("Header name to search for")] name: Str) -> R<Value> {
    let wanted = name.as_str().to_lowercase();
    Ok(response_headers(target)?
        .into_iter()
        .find(|(name, _)| name.to_lowercase() == wanted)
        .map_or(Value::Null, |(_, value)| Value::str(&value)))
}
