//! The native platform's HTTP client against local servers (plain HTTP, and HTTPS with a self-signed
//! certificate), and DNS lookups of `localhost`. Nothing leaves the machine.

use std::io::{BufRead, BufReader, Read, Write};
use std::net::TcpListener;
use std::rc::Rc;
use std::sync::Arc;
use std::thread;

use mash_core::eval::{Ctx, render, with_big_stack};
use mash_core::platform::{HttpRequest, HttpResponse, Platform};
use mash_os::NativePlatform;

/// A request as the server saw it
struct Received {
    method: String,
    path: String,
    headers: Vec<(String, String)>,
    body: String,
}

impl Received {
    fn read(stream: &mut impl Read) -> Option<Received> {
        let mut reader = BufReader::new(stream);
        let mut line = String::new();
        reader.read_line(&mut line).ok()?;
        let mut parts = line.split_whitespace();
        let method = parts.next()?.to_string();
        let path = parts.next()?.to_string();
        let mut headers = vec![];
        loop {
            let mut line = String::new();
            reader.read_line(&mut line).ok()?;
            let line = line.trim_end();
            if line.is_empty() {
                break;
            }
            let (name, value) = line.split_once(':')?;
            headers.push((name.to_ascii_lowercase(), value.trim().to_string()));
        }
        let length = headers
            .iter()
            .find(|(name, _)| name == "content-length")
            .map_or(0, |(_, value)| value.parse().unwrap_or(0));
        let mut body = vec![0; length];
        reader.read_exact(&mut body).ok()?;
        Some(Received {
            method,
            path,
            headers,
            body: String::from_utf8_lossy(&body).into_owned(),
        })
    }

    /// The response the test server gives: by path
    fn response(&self) -> String {
        let respond = |status: &str, headers: &[&str], body: &str| {
            let mut out = format!(
                "HTTP/1.1 {status}\r\nContent-Length: {}\r\nConnection: close\r\n",
                body.len()
            );
            for header in headers {
                out.push_str(header);
                out.push_str("\r\n");
            }
            out.push_str("\r\n");
            out.push_str(body);
            out
        };
        if let Some(code) = self.path.strip_prefix("/status/") {
            return respond(&format!("{code} Whatever"), &[], "oops!");
        }
        if let Some(target) = self.path.strip_prefix("/redirect-to/") {
            let location = format!("Location: {target}");
            return respond("302 Found", &[&location], "");
        }
        match self.path.as_str() {
            "/redirect" => respond(
                "302 Found",
                &["Location: /echo?redirected=1", "Set-Cookie: r=1; Path=/"],
                "",
            ),
            "/cookie" => respond(
                "200 OK",
                &[
                    "Set-Cookie: theme=dark",
                    "Set-Cookie: session=abc; Path=/",
                    "X-Multi: 1",
                    "X-Multi: 2",
                    "Content-Type: application/json",
                ],
                r#"{"ok": true}"#,
            ),
            _ => {
                // Echo the request: method, path, headers (name: value) and body
                let mut text = format!("{} {}\n", self.method, self.path);
                for (name, value) in &self.headers {
                    text.push_str(&format!("{name}: {value}\n"));
                }
                text.push('\n');
                text.push_str(&self.body);
                respond("200 OK", &["Content-Type: text/plain"], &text)
            }
        }
    }
}

fn serve(mut stream: impl Read + Write) {
    if let Some(request) = Received::read(&mut stream) {
        let _ = stream.write_all(request.response().as_bytes());
        let _ = stream.flush();
    }
}

/// Start a plain HTTP server on a free local port, returning its base URL
fn http_server() -> String {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let port = listener.local_addr().unwrap().port();
    thread::spawn(move || {
        for stream in listener.incoming().flatten() {
            thread::spawn(move || serve(stream));
        }
    });
    format!("http://127.0.0.1:{port}")
}

/// Start an HTTPS server with a self-signed certificate for `localhost`, returning its base URL
fn https_server() -> String {
    let certified = rcgen::generate_simple_self_signed(vec!["localhost".to_string()]).unwrap();
    let cert = certified.cert.der().clone();
    let key = rustls::pki_types::PrivateKeyDer::Pkcs8(rustls::pki_types::PrivatePkcs8KeyDer::from(
        certified.signing_key.serialize_der(),
    ));
    let config = rustls::ServerConfig::builder_with_provider(Arc::new(rustls::crypto::ring::default_provider()))
        .with_safe_default_protocol_versions()
        .unwrap()
        .with_no_client_auth()
        .with_single_cert(vec![cert], key)
        .unwrap();
    let config = Arc::new(config);
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let port = listener.local_addr().unwrap().port();
    thread::spawn(move || {
        for stream in listener.incoming().flatten() {
            let config = config.clone();
            thread::spawn(move || {
                let Ok(connection) = rustls::ServerConnection::new(config) else {
                    return;
                };
                serve(rustls::StreamOwned::new(connection, stream));
            });
        }
    });
    format!("https://localhost:{port}")
}

fn request(method: &str, url: &str) -> std::io::Result<HttpResponse> {
    NativePlatform.http_request(&HttpRequest {
        method,
        url,
        headers: &[],
        body: None,
        basic_auth: None,
        cookies: &[],
        insecure: false,
        timeout: None,
        interrupt: None,
    })
}

fn text(response: &HttpResponse) -> String {
    String::from_utf8_lossy(&response.body).into_owned()
}

/// Evaluate Mash code natively, rendering the result or the error
/// A server that accepts connections but never answers
fn silent_server() -> String {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let port = listener.local_addr().unwrap().port();
    thread::spawn(move || {
        let mut held = vec![];
        for stream in listener.incoming() {
            held.push(stream);
        }
    });
    format!("http://127.0.0.1:{port}/")
}

#[test]
fn a_request_that_takes_too_long_times_out() {
    let url = silent_server();
    let started = std::time::Instant::now();
    let result = eval(format!("http.get '{url}' --timeout=300"));
    assert_eq!(result, format!("error: Timed out after 300 milliseconds: {url}"));
    assert_eq!(
        eval(format!("json.get '{url}' --timeout=300")),
        format!("error: Timed out after 300 milliseconds: {url}")
    );
    assert!(started.elapsed() < std::time::Duration::from_secs(5));
    assert_eq!(
        eval(format!("http.get '{url}' --timeout='soon'")),
        "error: Invalid argument 'timeout'. Must be a duration, but was a String"
    );
}

#[test]
fn timeout_stops_a_request() {
    let url = silent_server();
    let started = std::time::Instant::now();
    assert_eq!(
        eval(format!("http.get '{url}' | timeout 300")),
        "error: Timed out after 300 milliseconds"
    );
    // The sooner of the request's own timeout and timeout's
    assert_eq!(
        eval(format!("http.get '{url}' --timeout=200 | timeout 5000")),
        format!("error: Timed out after 200 milliseconds: {url}")
    );
    assert!(started.elapsed() < std::time::Duration::from_secs(3));
}

#[test]
fn an_interrupt_abandons_a_request() {
    let url = silent_server();
    let interrupt = Arc::new(std::sync::atomic::AtomicBool::new(false));
    let setter = interrupt.clone();
    thread::spawn(move || {
        thread::sleep(std::time::Duration::from_millis(200));
        setter.store(true, std::sync::atomic::Ordering::Relaxed);
    });
    let started = std::time::Instant::now();
    let error = NativePlatform
        .http_request(&HttpRequest {
            method: "GET",
            url: &url,
            headers: &[],
            body: None,
            basic_auth: None,
            cookies: &[],
            insecure: false,
            timeout: None,
            interrupt: Some(&interrupt),
        })
        .unwrap_err();
    assert_eq!(error.kind(), std::io::ErrorKind::Interrupted);
    assert!(started.elapsed() < std::time::Duration::from_secs(5));
}

fn eval(code: String) -> String {
    with_big_stack(move || {
        let mut ctx = Ctx::new(Rc::new(NativePlatform));
        match ctx.run("test", &code) {
            Ok(value) => render(&value),
            Err(e) => format!("error: {e}"),
        }
    })
}

#[test]
fn get_sends_headers_cookies_and_basic_auth() {
    let base = http_server();
    let headers = vec![("X-One".to_string(), "1".to_string()), ("X-One".into(), "2".into())];
    let cookies = vec![("a".to_string(), "1".to_string()), ("b".into(), "2".into())];
    let response = NativePlatform
        .http_request(&HttpRequest {
            method: "GET",
            url: &format!("{base}/echo?q=1"),
            headers: &headers,
            body: None,
            basic_auth: Some(("u", "p")),
            cookies: &cookies,
            insecure: false,
            timeout: None,
            interrupt: None,
        })
        .unwrap();
    assert_eq!(response.status, 200);
    let echoed = text(&response);
    assert!(echoed.starts_with("GET /echo?q=1\n"), "{echoed}");
    for expected in [
        "x-one: 1\n",
        "x-one: 2\n",
        "cookie: a=1;b=2\n",
        "authorization: Basic dTpw\n",
    ] {
        assert!(echoed.contains(expected), "{expected:?} not in {echoed}");
    }
    assert!(
        response
            .headers
            .contains(&("Content-Type".to_string(), "text/plain".to_string()))
    );
}

#[test]
fn bodies_are_sent() {
    let base = http_server();
    for method in ["POST", "PUT", "PATCH"] {
        let response = NativePlatform
            .http_request(&HttpRequest {
                method,
                url: &format!("{base}/echo"),
                headers: &[("Content-Type".into(), "application/json".into())],
                body: Some(br#"{"a": 1}"#),
                basic_auth: None,
                cookies: &[],
                insecure: false,
                timeout: None,
                interrupt: None,
            })
            .unwrap();
        let echoed = text(&response);
        assert!(echoed.starts_with(&format!("{method} /echo\n")), "{echoed}");
        assert!(echoed.contains("content-type: application/json\n"), "{echoed}");
        assert!(echoed.ends_with("\n\n{\"a\": 1}"), "{echoed}");
    }
    let response = request("DELETE", &format!("{base}/echo")).unwrap();
    assert!(text(&response).starts_with("DELETE /echo\n"));
}

#[test]
fn error_statuses_are_responses() {
    let base = http_server();
    for status in [404, 500] {
        let response = request("GET", &format!("{base}/status/{status}")).unwrap();
        assert_eq!(response.status, status);
        assert_eq!(text(&response), "oops!");
    }
}

#[test]
fn cookies_and_repeated_headers() {
    let base = http_server();
    let response = request("GET", &format!("{base}/cookie")).unwrap();
    // Sorted by name, as Apache's cookie store kept them
    assert_eq!(
        response.cookies,
        vec![
            ("session".to_string(), "abc".to_string()),
            ("theme".into(), "dark".into())
        ]
    );
    let multi: Vec<&str> = response
        .headers
        .iter()
        .filter(|(name, _)| name == "X-Multi")
        .map(|(_, value)| value.as_str())
        .collect();
    assert_eq!(multi, ["1", "2"]);
}

#[test]
fn redirects_are_followed_keeping_cookies() {
    let base = http_server();
    let response = request("GET", &format!("{base}/redirect")).unwrap();
    assert_eq!(response.status, 200);
    let echoed = text(&response);
    assert!(echoed.starts_with("GET /echo?redirected=1\n"), "{echoed}");
    // The cookie set by the redirect is sent to the new location, and is among the response's cookies
    assert!(echoed.contains("cookie: r=1\n"), "{echoed}");
    assert_eq!(response.cookies, vec![("r".to_string(), "1".to_string())]);

    // As with Apache's HTTP client, a POST isn't redirected by a 302 (but its cookies are kept)
    let response = NativePlatform
        .http_request(&HttpRequest {
            method: "POST",
            url: &format!("{base}/redirect"),
            headers: &[],
            body: Some(b"x"),
            basic_auth: None,
            cookies: &[],
            insecure: false,
            timeout: None,
            interrupt: None,
        })
        .unwrap();
    assert_eq!(response.status, 302);
    assert_eq!(response.cookies, vec![("r".to_string(), "1".to_string())]);
}

#[test]
fn credentials_are_not_sent_on_to_another_origin() {
    let base = http_server();
    let other = http_server();
    let headers = vec![
        ("Authorization".to_string(), "Bearer t".to_string()),
        ("X-Api".into(), "k".into()),
    ];
    let cookies = vec![("a".to_string(), "1".to_string())];
    let send = |url: String, basic_auth| {
        let response = NativePlatform
            .http_request(&HttpRequest {
                method: "GET",
                url: &url,
                headers: &headers,
                body: None,
                basic_auth,
                cookies: &cookies,
                insecure: false,
                timeout: None,
                interrupt: None,
            })
            .unwrap();
        text(&response)
    };
    // Within the origin, they're kept
    let echoed = send(format!("{base}/redirect-to/{base}/echo"), Some(("u", "p")));
    for expected in ["authorization: Basic dTpw\n", "cookie: a=1\n", "x-api: k\n"] {
        assert!(echoed.contains(expected), "{expected:?} not in {echoed}");
    }
    // Once a redirect leaves it (another port is another origin), they aren't sent; other headers still are
    for basic_auth in [Some(("u", "p")), None] {
        let echoed = send(format!("{base}/redirect-to/{other}/echo"), basic_auth);
        assert!(echoed.starts_with("GET /echo\n"), "{echoed}");
        assert!(!echoed.contains("authorization:"), "{echoed}");
        assert!(!echoed.contains("cookie:"), "{echoed}");
        assert!(echoed.contains("x-api: k\n"), "{echoed}");
    }
    // Nor are they sent back if a later redirect returns to the original origin
    let echoed = send(
        format!("{base}/redirect-to/{other}/redirect-to/{base}/echo"),
        Some(("u", "p")),
    );
    assert!(!echoed.contains("authorization:"), "{echoed}");
}

#[test]
fn connection_refused_is_a_clean_error() {
    let port = {
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        listener.local_addr().unwrap().port()
    };
    let error = request("GET", &format!("http://127.0.0.1:{port}/")).unwrap_err();
    assert_eq!(
        error.to_string(),
        format!("Couldn't connect to 127.0.0.1:{port}: Connection refused")
    );
}

#[test]
fn untrusted_certificates_are_refused_unless_insecure() {
    let base = https_server();
    let url = format!("{base}/echo");
    let error = request("GET", &url).unwrap_err().to_string();
    assert!(
        error.contains("certificate") && error.ends_with("(--insecure skips certificate verification)"),
        "unexpected error: {error}"
    );
    let response = NativePlatform
        .http_request(&HttpRequest {
            method: "GET",
            url: &url,
            headers: &[],
            body: None,
            basic_auth: None,
            cookies: &[],
            insecure: true,
            timeout: None,
            interrupt: None,
        })
        .unwrap();
    assert!(text(&response).starts_with("GET /echo\n"));

    // The same through Mash
    assert!(eval(format!("http.get '{url}'")).starts_with("error: "));
    assert_eq!(eval(format!("(http.get '{url}' --insecure).status")), "200");
}

#[test]
fn mash_http_functions() {
    let base = http_server();
    assert_eq!(
        eval(format!(
            "http.get '{base}/cookie' | [.status, .cookies.name, .getHeaders 'x-multi', .json.ok, .succeeded]"
        )),
        "[200, [session, theme], [1, 2], true, true]"
    );
    assert_eq!(eval(format!("json.get '{base}/cookie'")), "{ ok: true }");
    let echoed = eval(format!(
        "(http.post '{base}/echo' {{ a: 1 }} --json --queryParams={{ q: 'a b' }}).body"
    ));
    assert!(echoed.starts_with("POST /echo?q=a+b\n"), "{echoed}");
    assert!(echoed.contains("accept: application/json\n"), "{echoed}");
    assert!(echoed.ends_with("\n\n{\n  \"a\": 1\n}"), "{echoed}");
    assert_eq!(eval(format!("(http.get '{base}/status/404').succeeded")), "false");
}

#[test]
fn localhost_resolves() {
    let addresses = NativePlatform.lookup_host("localhost").unwrap();
    assert!(addresses.contains(&"127.0.0.1".to_string()), "{addresses:?}");
    // IPv4 first, as Java lists them
    assert!(!addresses[0].contains(':'), "{addresses:?}");
}
