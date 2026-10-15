//! http, the HTTP functions of json, dns and `Url.open` against `TestPlatform`'s canned responses; and the file
//! functions of the data namespaces (`json.read`/`write`, `xml.read`, `csv.write`, `hash.sha256` of a file,
//! `mash.source`, `mash.reload`) against its in-memory files. Expected values were checked against Scala Mash
//! (with a local HTTP server); comments mark deliberate differences.

use std::rc::Rc;

use mash_core::eval::{Ctx, RunError, render, with_big_stack};
use mash_core::platform::{HttpRecord, HttpResponse, MemoryFiles, TestPlatform};

const ECHO: &str = r#"{ "method": "GET", "n": [1, 2.5], "ok": true }"#;

fn platform() -> TestPlatform {
    TestPlatform {
        files: MemoryFiles::of(&["/home/alice/"])
            .with_file("/home/alice/data.json", r#"{ "a": [1, 2], "b": { "c": null } }"#)
            .with_file("/home/alice/data.xml", "<a><b x='1'>text</b><b>more</b></a>")
            .with_file(
                "/home/alice/data.csv",
                "name,age,city\nAda,36,\"London, UK\"\nBob,,Paris\n",
            )
            .with_file("/home/alice/ragged.csv", "a,b\n1,2\n3\n")
            .with_file(
                "/home/alice/data.toml",
                "[package]\nname = \"demo\"\nwhen = 2026-10-15T10:30:00+02:00\nlocal = 2026-10-15T10:30:00\nday = 2026-10-15\ntime = 10:30:00\nbig = 9007199254740993\nratio = inf\n\n[[bin]]\nname = \"a\"\n\n[[bin]]\nname = \"b\"\n",
            )
            .with_file("/home/alice/multi.yaml", "name: a\n---\nname: b\n")
            .with_file(
                "/home/alice/data.yaml",
                "country: NO\nenabled: on\nversion: 1.10\nwhen: 2026-10-15\n1: one\nref: !Ref MyBucket\nbase: &b {x: 1, y: 1}\nderived:\n  <<: *b\n  y: 2\n# a comment\n",
            )
            .with_file("/home/alice/body.txt", "from a file")
            .with_file("/home/alice/script.mash", "def triple n = n * 3\nsourced = triple 14")
            .with_file("/home/alice/bad.mash", "1 +"),
        ..TestPlatform::default()
    }
    .with_http_response("GET", "http://example.com/echo", HttpResponse::new(200, ECHO))
    .with_http_response("POST", "http://example.com/echo", HttpResponse::new(201, ECHO))
    .with_http_response("PUT", "http://example.com/echo", HttpResponse::new(200, ECHO))
    .with_http_response("PATCH", "http://example.com/echo", HttpResponse::new(200, ECHO))
    .with_http_response("DELETE", "http://example.com/echo", HttpResponse::new(204, ""))
    .with_http_response("DELETE", "http://example.com/echo?a=1", HttpResponse::new(200, "{}"))
    .with_http_response(
        "GET",
        "http://example.com/cookie",
        HttpResponse::new(200, "{}")
            .with_header("Set-Cookie", "session=abc")
            .with_header("X-Multi", "1")
            .with_header("X-Multi", "2")
            .with_header("Content-Type", "application/json")
            .with_cookie("session", "abc")
            .with_cookie("theme", "dark"),
    )
    .with_http_response("GET", "http://example.com/missing", HttpResponse::new(404, "oops!"))
    .with_http_response(
        "GET",
        "http://example.com/q?x=1&q=a+b&q=2&%C3%A9%26=%2F%3F",
        HttpResponse::new(200, "[]"),
    )
}

/// Run code, returning the rendered result (or the error) and the platform afterwards
fn run_with(platform: TestPlatform, code: &str) -> (String, Rc<TestPlatform>) {
    let platform = Rc::new(platform);
    let mut ctx = Ctx::new(platform.clone());
    let result = match ctx.run("test", code) {
        Ok(value) => render(&value),
        Err(RunError::Eval(e)) => format!("error: {e}"),
        Err(e) => format!("error: {e}"),
    };
    (result, platform)
}

/// Check each `(code, expected rendering)` pair, in a fresh interpreter each
fn check(cases: &'static [(&'static str, &'static str)]) {
    with_big_stack(move || {
        let mut failures = vec![];
        for (code, expected) in cases {
            let actual = run_with(platform(), code).0;
            if actual != *expected {
                failures.push(format!("{code:?}: got {actual:?}, expected {expected:?}"));
            }
        }
        assert!(
            failures.is_empty(),
            "{} failing:\n  {}",
            failures.len(),
            failures.join("\n  ")
        );
    })
}

/// The requests made by running `code`
fn requests(code: &'static str) -> Vec<HttpRecord> {
    with_big_stack(move || {
        let (result, platform) = run_with(platform(), code);
        assert!(!result.starts_with("error"), "{code}: {result}");
        platform.http_requests()
    })
}

fn headers(pairs: &[(&str, &str)]) -> Vec<(String, String)> {
    pairs.iter().map(|(n, v)| (n.to_string(), v.to_string())).collect()
}

#[test]
fn responses() {
    check(&[
        (
            "http.get 'http://example.com/cookie' | [.status, .cookies, .headers.name, .getHeaders 'x-multi', .succeeded]",
            "[200, [{ name: session, value: abc }, { name: theme, value: dark }], [Set-Cookie, X-Multi, X-Multi, Content-Type], [1, 2], true]",
        ),
        (
            "(http.get 'http://example.com/cookie').headersObject",
            "{ Set-Cookie: session=abc, X-Multi: 2, Content-Type: application/json }",
        ),
        (
            "(http.get 'http://example.com/cookie').getHeader 'CONTENT-type'",
            "application/json",
        ),
        ("(http.get 'http://example.com/cookie').getHeader 'nope'", "null"),
        ("(http.get 'http://example.com/cookie').getClass", "http.Response"),
        (
            "(http.get 'http://example.com/cookie').headers.first.getClass",
            "http.Header",
        ),
        (
            "(http.get 'http://example.com/cookie').cookies.first.getClass",
            "http.Cookie",
        ),
        (
            "http.get 'http://example.com/missing' | [.status, .body, .succeeded]",
            "[404, oops!, false]",
        ),
        ("(http.get 'http://example.com/echo').json.n", "[1, 2.5]"),
        ("(http.post 'http://example.com/echo').status", "201"),
        (
            "(http.delete 'http://example.com/echo') | [.status, .body]",
            "[204, \"\"]",
        ),
        ("http.Header.new 'a' 'b'", "{ name: a, value: b }"),
        ("http.Response.new 299 '' [] [] | .succeeded", "true"),
        (
            "json.get 'http://example.com/echo'",
            "{ method: GET, n: [1, 2.5], ok: true }",
        ),
        ("json.post 'http://example.com/echo' { a: 1 } | .ok", "true"),
        ("json.put 'http://example.com/echo' | .ok", "true"),
        ("json.patch 'http://example.com/echo' | .ok", "true"),
        (
            "http.get 'http://example.com/q?x=1' --queryParams={ q: ['a b', 2], 'é&': '/?' } | .status",
            "200",
        ),
        // Scala Mash's lenient JSON parser read `oops!` as a string
        (
            "json.get 'http://example.com/missing'",
            "error: Invalid JSON: expected value at line 1 column 1",
        ),
    ]);
}

#[test]
fn request_errors() {
    check(&[
        (
            "http.get 'http://nowhere.example/'",
            "error: Unknown host: no canned response for GET http://nowhere.example/",
        ),
        // Scala Mash: "Unexpected error … URISyntaxException" and "… ClientProtocolException"
        (
            "http.get 'not a url'",
            "error: Invalid URL: Illegal character in path at index 3: not a url",
        ),
        (
            "http.get 'foo'",
            "error: Invalid URL 'foo': must be an absolute http or https URL",
        ),
        (
            "http.get 'http://example.com/echo' --headers=['bad']",
            "error: Invalid argument 'headers'. Header string must be 'name:value'",
        ),
        (
            "http.get 'http://example.com/echo' --headers=[':x']",
            "error: Invalid argument 'headers'. Header string must be 'name:value'",
        ),
        // New: other values are errors, not silently no headers or cookies
        (
            "http.get 'http://example.com/echo' --headers=42",
            "error: Invalid argument 'headers'. Must be an object, a list or a string of headers, but was a Number",
        ),
        (
            "http.get 'http://example.com/echo' --cookies='a=1'",
            "error: Invalid argument 'cookies'. Must be an object of cookies, but was a String",
        ),
        (
            "http.get 'http://example.com/echo' --headers=[42]",
            "error: Invalid argument 'headers'. Invalid header of type Number",
        ),
        (
            "http.get 'http://example.com/echo' --headers=[{ name: 'a' }]",
            "error: Invalid argument 'headers'. Header object must have 'value' field",
        ),
        (
            "http.get 'http://example.com/echo' --basicAuth='bad'",
            "error: Invalid argument 'basicAuth'. Auth string must be 'username:password'",
        ),
        (
            "http.get 'http://example.com/echo' --basicAuth=42",
            "error: Invalid argument 'basicAuth'. Invalid authorisation of type Number",
        ),
        (
            "http.get 'http://example.com/echo' --basicAuth={ username: 'u' }",
            "error: Invalid argument 'basicAuth'. Auth object must have 'password' field",
        ),
        (
            "http.post 'http://example.com/echo' 'raw' --form",
            "error: Invalid argument 'form'. Invalid body, must be an object",
        ),
        (
            "http.post 'http://example.com/echo' {} --form --json",
            "error: Invalid argument 'json'. Incompatible arguments json and form",
        ),
        (
            "http.post 'http://example.com/echo' 'nope.txt' --file",
            "error: Could not read 'nope.txt': No such file or directory: nope.txt",
        ),
    ]);
}

#[test]
fn request_headers_cookies_and_auth() {
    let sent = requests(
        "http.get 'http://example.com/echo' --headers=['A:1', 'A:2'] --cookies={ c: 1, d: 2 } --basicAuth='u:p' --json",
    );
    // Scala Mash sent only the last of several headers with the same name (`A: 2`), which defeated its
    // support for several values
    assert_eq!(
        sent[0].headers,
        headers(&[
            ("Accept", "application/json"),
            ("A", "1"),
            ("A", "2"),
            ("Cookie", "c=1;d=2"),
            ("Authorization", "Basic dTpw"),
        ])
    );
    assert_eq!(sent[0].method, "GET");
    assert_eq!(sent[0].body, None);
    assert!(!sent[0].insecure);

    let sent = requests(
        "http.get 'http://example.com/echo' --headers={ X: [1, 2], Y: 3 } --basicAuth={ username: 'u', password: 'p' } --insecure",
    );
    assert_eq!(
        sent[0].headers,
        headers(&[("X", "1"), ("X", "2"), ("Y", "3"), ("Authorization", "Basic dTpw")])
    );
    assert!(sent[0].insecure);

    let sent =
        requests("http.get 'http://example.com/echo' --headers=[{ name: 'Accept', value: 'text/plain' }] --json");
    assert_eq!(sent[0].headers, headers(&[("Accept", "text/plain")]));

    // New: a single "name:value" string is a header; split at the first colon (values can contain more), and
    // trimmed (Scala ignored strings, and rejected values with colons)
    let sent = requests("http.get 'http://example.com/echo' --headers='X-Url: http://a/b'");
    assert_eq!(sent[0].headers, headers(&[("X-Url", "http://a/b")]));
    let sent = requests("http.get 'http://example.com/echo' --headers=['At: 10:30', 'B:1'] --basicAuth='u:p:q'");
    assert_eq!(
        sent[0].headers,
        headers(&[("At", "10:30"), ("B", "1"), ("Authorization", "Basic dTpwOnE=")])
    );
}

#[test]
fn request_bodies() {
    let sent = requests("http.post 'http://example.com/echo' { a: 1 } --json");
    assert_eq!(sent[0].method, "POST");
    assert_eq!(sent[0].body.as_deref(), Some("{\n  \"a\": 1\n}"));
    assert_eq!(
        sent[0].headers,
        headers(&[
            ("Accept", "application/json"),
            ("Content-Type", "application/json; charset=UTF-8")
        ])
    );

    let sent = requests("http.post 'http://example.com/echo' { a: 1, b: 'x y', 'é': '&' } --form");
    assert_eq!(sent[0].body.as_deref(), Some("a=1&b=x+y&%C3%A9=%26"));
    assert_eq!(
        sent[0].headers,
        headers(&[("Content-Type", "application/x-www-form-urlencoded")])
    );

    let sent = requests("http.put 'http://example.com/echo' 'raw'");
    assert_eq!((sent[0].method.as_str(), sent[0].body.as_deref()), ("PUT", Some("raw")));
    assert_eq!(
        sent[0].headers,
        headers(&[("Content-Type", "application/octet-stream")])
    );

    let sent =
        requests("http.patch 'http://example.com/echo' 'body.txt' --file --headers={ 'content-type': 'text/plain' }");
    assert_eq!(sent[0].method, "PATCH");
    assert_eq!(sent[0].body.as_deref(), Some("from a file"));
    assert_eq!(sent[0].headers, headers(&[("content-type", "text/plain")]));

    let sent = requests("http.post 'http://example.com/echo'");
    assert_eq!(sent[0].body.as_deref(), Some(""));

    let sent = requests("json.post 'http://example.com/echo' { a: [1, 2.5] }");
    assert_eq!(sent[0].body.as_deref(), Some("{\n  \"a\": [\n    1,\n    2.5\n  ]\n}"));
    assert_eq!(
        sent[0].headers[0],
        ("Accept".to_string(), "application/json".to_string())
    );

    let sent = requests("json.delete 'http://example.com/echo' --queryParams={ a: 1 }");
    assert_eq!(sent[0].method, "DELETE");
    assert_eq!(sent[0].url, "http://example.com/echo?a=1");
}

#[test]
fn query_parameters() {
    let sent = requests("http.get 'http://example.com/q?x=1' --queryParams={ q: ['a b', 2], 'é&': '/?' }");
    assert_eq!(sent[0].url, "http://example.com/q?x=1&q=a+b&q=2&%C3%A9%26=%2F%3F");
}

#[test]
fn dns_and_urls() {
    check(&[
        ("dns.lookup 'localhost'", "[127.0.0.1, ::1]"),
        (
            "(net.url 'http://localhost:8080/x').host.ipAddresses",
            "[127.0.0.1, ::1]",
        ),
        ("dns.lookup 'nowhere.example'", "error: Unknown host: nowhere.example"),
    ]);
    with_big_stack(|| {
        let (result, after) = run_with(platform(), "(net.url 'http://example.com/a b').open");
        assert_eq!(
            result,
            "error: Invalid argument 'url'. Illegal character in path at index 20: http://example.com/a b"
        );
        assert!(after.opened.borrow().is_empty());
        let (result, after) = run_with(platform(), "(net.url 'http://example.com/').open");
        assert_eq!(result, "");
        assert_eq!(*after.opened.borrow(), vec!["http://example.com/".to_string()]);
    });
}

#[test]
fn data_files() {
    check(&[
        ("json.read 'data.json'", "{ a: [1, 2], b: { c: null } }"),
        (
            "json.read 'nope.json'",
            "error: Could not read 'nope.json': No such file or directory: nope.json",
        ),
        (
            "json.write 'out.json' { a: [1, 2], b: {}, c: [] }; read 'out.json'",
            "{\n  \"a\": [\n    1,\n    2\n  ],\n  \"b\": {},\n  \"c\": []\n}",
        ),
        ("json.write 'out.json' 42; json.read 'out.json'", "42"),
        ("xml.read 'data.xml'", "{ a: { b: [{ x: 1, content: text }, more] } }"),
        (
            "xml.read 'nope.xml'",
            "error: Could not read 'nope.xml': No such file or directory: nope.xml",
        ),
        (
            "csv.write 'out.csv' [{ a: 1, b: 'x,y' }, { c: '', a: '\"q\"' }, { a: ' lead', b: '#c' }]; read 'out.csv'",
            "a,b,c\r\n1,\"x,y\",\r\n\"\"\"q\"\"\",,\r\n\" lead\",\"#c\",\r\n",
        ),
        // New: null is written as no value, as a missing field is (Scala wrote `null`)
        (
            "csv.write 'out.csv' [{ a: 1, b: null }, { a: null, b: 2 }]; read 'out.csv'",
            // (an empty first value is quoted, as Commons CSV does, so that the line isn't empty)
            "a,b\r\n1,\r\n\"\",2\r\n",
        ),
        ("csv.write 'out.csv' []; read 'out.csv'", "\r\n"),
        // New: reading CSV, a header line then one object per line, every value a string
        (
            "csv.read 'data.csv'",
            "[{ name: Ada, age: 36, city: \"London, UK\" }, { name: Bob, age: \"\", city: Paris }]",
        ),
        ("csv.read 'data.csv' | map (.age.getClass.name)", "[String, String]"),
        ("'x,y\n1,\"a\nb\"' | csv.fromString | map (.y)", "[\"a\\nb\"]"),
        ("'' | csv.fromString", "[]"),
        (
            "csv.write 'out.csv' [{ a: 1, b: 'x,y' }, { a: null, b: '\"q\"' }]; csv.read 'out.csv'",
            "[{ a: 1, b: \"x,y\" }, { a: \"\", b: \"\\\"q\\\"\" }]",
        ),
        ("csv.write 'out.csv' []; csv.read 'out.csv'", "[]"),
        (
            "csv.read 'ragged.csv'",
            "error: Invalid CSV: line 3 has 1 value, but the header has 2",
        ),
        (
            "csv.read 'nope.csv'",
            "error: Could not read 'nope.csv': No such file or directory: nope.csv",
        ),
        (
            "csv.write 'out.csv' ['x']",
            "error: Invalid argument 'values'. Values to write must be objects, but found: String",
        ),
        (
            "csv.write 'out.csv' 'abc'",
            "error: Invalid argument 'values'. Must be a sequence, but was a String",
        ),
        // Hashing a path hashes the file's contents; a plain string is hashed itself
        (
            "hash.sha256 ('body.txt'.toPath)",
            "552b49dcb23b4558021df8fda10272de45986bce69dd80283fc0b2a2fa57c209",
        ),
        (
            "hash.sha256 (ls 'body.txt' | first)",
            "552b49dcb23b4558021df8fda10272de45986bce69dd80283fc0b2a2fa57c209",
        ),
        (
            "hash.sha256 'body.txt'",
            "7c7247198c247fa236390f0a07ddd8c0ec587596b690a031ae089864ec2a8fbd",
        ),
        (
            "hash.sha256 ('nope'.toPath)",
            "error: Could not read 'nope': No such file or directory: nope",
        ),
    ]);
}

/// (The Scala oracle can't run these: they need the REPL's singletons. Expected values follow Scala's
/// `SourceFunction` and `ReloadFunction`.)
#[test]
fn yaml_files() {
    check(&[
        // YAML 1.2: no, on and yes are strings; keys become strings; custom tags are dropped; merge keys are
        // resolved, in order, the mapping's own fields winning
        (
            "yaml.read 'data.yaml'",
            "{ country: NO, enabled: on, version: 1.1, when: 2026-10-15, 1: one, ref: MyBucket, base: { x: 1, y: 1 }, derived: { x: 1, y: 2 } }",
        ),
        (
            "yaml.read 'data.yaml' | .fields.name | map (.getClass.name)",
            "[String, String, String, String, String, String, String, String]",
        ),
        // Several documents give a list of them; one that's a list is just that list
        ("yaml.read 'multi.yaml'", "[{ name: a }, { name: b }]"),
        ("yaml.fromString '- a`n- b'", "[a, b]"),
        ("yaml.fromString ''", "null"),
        ("yaml.fromString 'x: .inf'", "{ x: Infinity }"),
        (
            "yaml.fromString 'a: ['",
            "error: Invalid YAML: while parsing a node, did not find expected node content at byte 4 line 2 column 1",
        ),
        (
            "yaml.read 'nope.yaml'",
            "error: Could not read 'nope.yaml': No such file or directory: nope.yaml",
        ),
        // Strings that would read back as something else are quoted
        (
            "yaml.toString { country: 'NO', n: 1.5, s: '123', list: [1, { a: null }], empty: {}, t: true }",
            "country: \"NO\"\n\"n\": 1.5\ns: \"123\"\nlist:\n  - 1\n  - a: ~\nempty: {}\nt: true\n",
        ),
        ("yaml.toString [1, 2]", "- 1\n- 2\n"),
        ("yaml.toString 'plain'", "plain\n"),
        (
            "yaml.write 'out.yaml' (yaml.read 'data.yaml'); yaml.read 'out.yaml' | .derived",
            "{ x: 1, y: 2 }",
        ),
        (
            "yaml.write 'out.yaml' { a: [1, 2] }; read 'out.yaml'",
            "a:\n  - 1\n  - 2\n",
        ),
    ]);
}

#[test]
fn toml_files() {
    check(&[
        // Dates as DateTimes (without an offset, in the local time zone, here UTC), Dates and strings
        (
            "toml.read 'data.toml'",
            "{ package: { name: demo, when: 2026-10-15T08:30:00+00:00, local: 2026-10-15T10:30:00+00:00, day: 2026-10-15, time: 10:30:00, big: 9007199254740992, ratio: Infinity }, bin: [{ name: a }, { name: b }] }",
        ),
        (
            "toml.read 'data.toml' | .package | .fields | map (.value.getClass.name)",
            "[String, DateTime, DateTime, Date, String, Number, Number]",
        ),
        ("toml.fromString ''", "{}"),
        (
            "toml.fromString 'a = '",
            "error: Invalid TOML: string values must be quoted, expected literal string",
        ),
        (
            "toml.read 'nope.toml'",
            "error: Could not read 'nope.toml': No such file or directory: nope.toml",
        ),
        // Null fields are left out; lists of objects are arrays of tables; dates are written as dates
        (
            "toml.toString { name: 'x', gone: null, when: time.parseIso8601 '2026-10-15T10:30:00Z', day: '2026-10-15'.toDate, n: 1.5, mixed: [1, 'two'], bin: [{ name: 'a' }, { name: 'b' }] }",
            "name = \"x\"\nwhen = 2026-10-15T10:30:00Z\nday = 2026-10-15\nn = 1.5\nmixed = [1, \"two\"]\n\n[[bin]]\nname = \"a\"\n\n[[bin]]\nname = \"b\"\n",
        ),
        (
            "toml.toString [1]",
            "error: Cannot write a List as TOML: a TOML document is a table, so must be an object",
        ),
        (
            "toml.toString { a: [1, null] }",
            "error: Cannot write null in a list as TOML, as TOML has no null",
        ),
        (
            "toml.write 'out.toml' (toml.read 'data.toml'); toml.read 'out.toml' | .package | [.when, .day, .time]",
            "[2026-10-15T08:30:00+00:00, 2026-10-15, 10:30:00]",
        ),
    ]);
}

#[test]
fn source_and_reload() {
    check(&[
        ("mash.source 'script.mash'; [sourced, triple 2]", "[42, 6]"),
        ("mash.source 'script.mash'", "42"),
        (
            "mash.source 'nope.mash'",
            "error: Could not read 'nope.mash': No such file or directory: nope.mash",
        ),
        // Scala Mash printed the error and returned unit
        (
            "mash.source 'bad.mash'",
            "error: Syntax error: Unexpected end of input, expected expression",
        ),
        // Without a host hook, reload just restores the standard globals (user variables stay)
        (
            "def length x = 'mine'; x = 1; mash.reload; [length [1, 2], x]",
            "[2, 1]",
        ),
    ]);
}
