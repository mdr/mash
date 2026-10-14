//! HTTP (with `ureq` and rustls), DNS lookups, and opening things with the default application.

use std::io;
use std::net::ToSocketAddrs;
use std::process::{Command, Stdio};
use std::sync::atomic::Ordering;
use std::sync::mpsc::{self, RecvTimeoutError};
use std::thread;
use std::time::{Duration, Instant};

use mash_core::platform::{HttpRequest, HttpResponse};
use ureq::http::{self, Uri};
use ureq::tls::{RootCerts, TlsConfig};
use ureq::{Agent, Error};

/// Redirects followed before giving up (Apache's HTTP client's default)
const MAX_REDIRECTS: usize = 50;

/// The agent for a request: certificates checked against the system's trust store (unless `insecure`), and any
/// status is a response. Redirects are followed by [`http_request`] itself, which also keeps the cookies.
pub(crate) fn agent(insecure: bool, timeout: Option<Duration>) -> Agent {
    let tls = TlsConfig::builder()
        .root_certs(RootCerts::PlatformVerifier)
        .disable_verification(insecure)
        .build();
    Agent::config_builder()
        .http_status_as_error(false)
        .max_redirects(0)
        .tls_config(tls)
        .timeout_global(timeout)
        .build()
        .into()
}

/// A cookie a server set: the host that set it, its name and value
type Cookie = (String, String, String);

/// Make a request, following redirects as Apache's HTTP client (which Scala Mash used) does: a `GET` or `HEAD`
/// is redirected by 301, 302, 303, 307 and 308; any method by 303, as a `GET`. Cookies set along the way are
/// sent on to the same host, and are the response's cookies. Once a redirect leaves the original origin (scheme,
/// host and port), the credentials given with the request (`Authorization`, `Proxy-Authorization` and `Cookie`
/// headers, including basic authentication) aren't sent any more, as curl and Python's requests do.
/// Make a request (as [`http_request`]) on another thread, so that an interrupt needn't wait for it: the request
/// is then abandoned, and finishes in the background (when the server answers or hangs up, or its timeout
/// passes)
pub(crate) fn interruptible_http_request(request: &HttpRequest<'_>) -> io::Result<HttpResponse> {
    const POLL_INTERVAL: Duration = Duration::from_millis(50);
    let Some(interrupt) = request.interrupt else {
        return http_request(request);
    };
    let owned = OwnedRequest::of(request);
    let (sender, receiver) = mpsc::channel();
    thread::Builder::new()
        .name("http".into())
        .spawn(move || {
            let _ = sender.send(http_request(&owned.request()));
        })?;
    loop {
        match receiver.recv_timeout(POLL_INTERVAL) {
            Ok(result) => return result,
            Err(RecvTimeoutError::Timeout) if interrupt.load(Ordering::Relaxed) => {
                return Err(io::Error::new(io::ErrorKind::Interrupted, "Interrupted"));
            }
            Err(RecvTimeoutError::Timeout) => {}
            Err(RecvTimeoutError::Disconnected) => return Err(io::Error::other("The HTTP request failed")),
        }
    }
}

/// An [`HttpRequest`] that can be sent to another thread
struct OwnedRequest {
    method: String,
    url: String,
    headers: Vec<(String, String)>,
    body: Option<Vec<u8>>,
    basic_auth: Option<(String, String)>,
    cookies: Vec<(String, String)>,
    insecure: bool,
    timeout: Option<Duration>,
}

impl OwnedRequest {
    fn of(request: &HttpRequest<'_>) -> Self {
        OwnedRequest {
            method: request.method.into(),
            url: request.url.into(),
            headers: request.headers.to_vec(),
            body: request.body.map(<[u8]>::to_vec),
            basic_auth: request.basic_auth.map(|(user, password)| (user.into(), password.into())),
            cookies: request.cookies.to_vec(),
            insecure: request.insecure,
            timeout: request.timeout,
        }
    }

    fn request(&self) -> HttpRequest<'_> {
        HttpRequest {
            method: &self.method,
            url: &self.url,
            headers: &self.headers,
            body: self.body.as_deref(),
            basic_auth: self.basic_auth.as_ref().map(|(user, password)| (user.as_str(), password.as_str())),
            cookies: &self.cookies,
            insecure: self.insecure,
            timeout: self.timeout,
            interrupt: None,
        }
    }
}

pub(crate) fn http_request(request: &HttpRequest<'_>) -> io::Result<HttpResponse> {
    let deadline = request.timeout.map(|timeout| Instant::now() + timeout);
    let timed_out = || {
        io::Error::new(
            io::ErrorKind::TimedOut,
            format!("Timed out after {}: {}", describe_duration(request.timeout.unwrap_or_default()), request.url),
        )
    };
    let headers = request.all_headers();
    let mut method = request.method.to_string();
    let mut url = request.url.to_string();
    let mut body = request.body;
    let mut cookies: Vec<Cookie> = vec![];
    let mut first_origin = None;
    let mut left_origin = false;
    for _ in 0..=MAX_REDIRECTS {
        // What's left of the timeout, for this step
        let remaining = match deadline {
            Some(deadline) => match deadline.checked_duration_since(Instant::now()) {
                Some(remaining) if !remaining.is_zero() => Some(remaining),
                _ => return Err(timed_out()),
            },
            None => None,
        };
        let agent = agent(request.insecure, remaining);
        let uri: Uri = url
            .parse()
            .map_err(|e| invalid_input(format!("Invalid URL '{url}': {e}")))?;
        let host = uri.host().unwrap_or_default().to_string();
        let first_origin = first_origin.get_or_insert_with(|| origin(&uri));
        left_origin |= origin(&uri) != *first_origin;
        let mut builder = http::Request::builder().method(method.as_str()).uri(uri.clone());
        let sending_body = body.is_some();
        for (name, value) in &headers {
            let about_body = name.eq_ignore_ascii_case("content-type") || name.eq_ignore_ascii_case("content-length");
            let credential = ["authorization", "proxy-authorization", "cookie"]
                .iter()
                .any(|c| name.eq_ignore_ascii_case(c));
            if (sending_body || !about_body) && !(left_origin && credential) {
                builder = builder.header(name, value);
            }
        }
        let jar: Vec<String> = cookies
            .iter()
            .filter(|(cookie_host, _, _)| *cookie_host == host)
            .map(|(_, name, value)| format!("{name}={value}"))
            .collect();
        if !jar.is_empty() {
            builder = builder.header("Cookie", jar.join("; "));
        }
        let result = match body {
            Some(body) => builder.body(body).map_err(Error::from).and_then(|r| agent.run(r)),
            None => builder.body(()).map_err(Error::from).and_then(|r| agent.run(r)),
        };
        let mut response = match result {
            Err(Error::Timeout(_)) if deadline.is_some() => return Err(timed_out()),
            result => result.map_err(|e| describe_error(e, &uri))?,
        };
        let status = response.status().as_u16();
        let headers: Vec<(String, String)> = response
            .headers()
            .iter()
            .map(|(name, value)| {
                let value = String::from_utf8_lossy(value.as_bytes()).into_owned();
                (header_name(name.as_str()), value)
            })
            .collect();
        for (name, value) in &headers {
            if name.eq_ignore_ascii_case("set-cookie")
                && let Some((cookie_name, cookie_value)) = parse_set_cookie(value)
            {
                cookies.retain(|(h, n, _)| !(*h == host && *n == cookie_name));
                cookies.push((host.clone(), cookie_name, cookie_value));
            }
        }
        let location = headers
            .iter()
            .find(|(name, _)| name.eq_ignore_ascii_case("location"))
            .map(|(_, value)| value.clone());
        let redirect = match (status, location) {
            (303, Some(location)) => {
                if method != "HEAD" {
                    method = "GET".into();
                }
                body = None;
                Some(location)
            }
            (301 | 302 | 307 | 308, Some(location)) if method == "GET" || method == "HEAD" => Some(location),
            _ => None,
        };
        if let Some(location) = redirect {
            url = resolve_location(&uri, &location);
            continue;
        }
        let body = response
            .body_mut()
            .with_config()
            .limit(u64::MAX)
            .read_to_vec()
            .map_err(|e| match e {
                Error::Timeout(_) if deadline.is_some() => timed_out(),
                e => describe_error(e, &uri),
            })?;
        // Apache's cookie store kept cookies sorted by name
        let mut cookies: Vec<(String, String)> = cookies.into_iter().map(|(_, n, v)| (n, v)).collect();
        cookies.sort();
        return Ok(HttpResponse {
            status,
            headers,
            cookies,
            body,
        });
    }
    Err(io::Error::other(format!(
        "Too many redirects ({MAX_REDIRECTS}) from {}",
        request.url
    )))
}

/// The name and value of a `Set-Cookie` header (its attributes, such as `Path`, are ignored)
fn parse_set_cookie(header: &str) -> Option<(String, String)> {
    let pair = header.split(';').next()?;
    let (name, value) = pair.split_once('=')?;
    let name = name.trim();
    (!name.is_empty()).then(|| (name.to_string(), value.trim().trim_matches('"').to_string()))
}

/// A redirect's `Location` relative to the URL that gave it
/// A URL's origin: its scheme, host and port (the scheme's default if none is given)
fn origin(uri: &Uri) -> (String, String, Option<u16>) {
    let scheme = uri.scheme_str().unwrap_or("http").to_ascii_lowercase();
    let port = uri.port_u16().or(match scheme.as_str() {
        "http" => Some(80),
        "https" => Some(443),
        _ => None,
    });
    (scheme, uri.host().unwrap_or_default().to_ascii_lowercase(), port)
}

fn resolve_location(base: &Uri, location: &str) -> String {
    let scheme = base.scheme_str().unwrap_or("http");
    let authority = base.authority().map_or("", |a| a.as_str());
    if location.contains("://") {
        location.to_string()
    } else if let Some(rest) = location.strip_prefix("//") {
        format!("{scheme}://{rest}")
    } else if location.starts_with('/') {
        format!("{scheme}://{authority}{location}")
    } else {
        let path = base.path();
        let directory = &path[..path.rfind('/').map_or(0, |i| i + 1)];
        let directory = if directory.is_empty() { "/" } else { directory };
        format!("{scheme}://{authority}{directory}{location}")
    }
}

/// A header name as servers usually write it (`content-type` → `Content-Type`): the `http` crate keeps only
/// lower-case names
fn header_name(name: &str) -> String {
    name.split('-')
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                Some(first) => first.to_ascii_uppercase().to_string() + chars.as_str(),
                None => String::new(),
            }
        })
        .collect::<Vec<_>>()
        .join("-")
}

fn invalid_input(message: String) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidInput, message)
}

/// A ureq error as a message for the user (in the style of Scala Mash's "Unknown host: …" and "Couldn't
/// connect: …")
/// A duration as a person would write it: `10 seconds`, `1.5 seconds`, `500 milliseconds`
fn describe_duration(duration: Duration) -> String {
    let plural = |n: f64, unit: &str| format!("{n} {unit}{}", if n == 1.0 { "" } else { "s" });
    if duration < Duration::from_secs(1) {
        plural(duration.as_millis() as f64, "millisecond")
    } else {
        plural(duration.as_secs_f64(), "second")
    }
}

pub(crate) fn describe_error(error: Error, uri: &Uri) -> io::Error {
    let host = uri.host().unwrap_or_default();
    let authority = uri.authority().map_or(host, |a| a.as_str());
    match error {
        Error::HostNotFound => io::Error::new(io::ErrorKind::NotFound, format!("Unknown host: {host}")),
        Error::Io(e) => {
            let message = e.to_string();
            let message = message.split(" (os error ").next().unwrap_or_default();
            // The standard library's message when the host name can't be resolved
            match message.strip_prefix("failed to lookup address information: ") {
                Some(reason) => io::Error::new(io::ErrorKind::NotFound, format!("Unknown host: {host}: {reason}")),
                // rustls's verification failures arrive as I/O errors
                None if message.contains("certificate") => io::Error::new(
                    e.kind(),
                    format!("Couldn't connect to {authority}: {message} (--insecure skips certificate verification)"),
                ),
                None => io::Error::new(e.kind(), format!("Couldn't connect to {authority}: {message}")),
            }
        }
        Error::ConnectionFailed => io::Error::other(format!("Couldn't connect to {authority}")),
        Error::Rustls(e) => io::Error::other(format!(
            "TLS error connecting to {authority}: {e} (--insecure skips certificate verification)"
        )),
        Error::Timeout(timeout) => io::Error::new(
            io::ErrorKind::TimedOut,
            format!("Timed out ({timeout}) connecting to {authority}"),
        ),
        Error::BadUri(message) => invalid_input(format!("Invalid URL '{uri}': {message}")),
        Error::Http(e) => invalid_input(format!("Invalid request: {e}")),
        other => io::Error::other(format!("HTTP error ({authority}): {other}")),
    }
}

pub(crate) fn lookup_host(host: &str) -> io::Result<Vec<String>> {
    let unknown = || io::Error::new(io::ErrorKind::NotFound, format!("Unknown host: {host}"));
    let addresses = (host, 0).to_socket_addrs().map_err(|_| unknown())?;
    let mut ips: Vec<String> = vec![];
    for address in addresses {
        let ip = address.ip().to_string();
        if !ips.contains(&ip) {
            ips.push(ip);
        }
    }
    if ips.is_empty() {
        return Err(unknown());
    }
    // IPv4 addresses first, as Java's `InetAddress.getAllByName` (which Scala Mash used) lists them
    ips.sort_by_key(|ip| ip.contains(':'));
    Ok(ips)
}

/// Open with `open` (macOS) or `xdg-open`, waiting for it to finish (as Scala Mash does; its exit status is
/// ignored)
pub(crate) fn open_with_default_application(target: &str) -> io::Result<()> {
    let opener = if cfg!(target_os = "macos") { "open" } else { "xdg-open" };
    Command::new(opener)
        .arg(target)
        .stdin(Stdio::null())
        .status()
        .map(drop)
        .map_err(|e| io::Error::new(e.kind(), format!("Could not run {opener}: {e}")))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn redirect_locations() {
        let base: Uri = "http://example.com:8080/a/b?x=1".parse().unwrap();
        assert_eq!(resolve_location(&base, "https://other/x"), "https://other/x");
        assert_eq!(resolve_location(&base, "//other/x"), "http://other/x");
        assert_eq!(resolve_location(&base, "/x?y=2"), "http://example.com:8080/x?y=2");
        assert_eq!(resolve_location(&base, "c"), "http://example.com:8080/a/c");
    }

    #[test]
    fn set_cookie_headers() {
        assert_eq!(
            parse_set_cookie("session=abc; Path=/; HttpOnly"),
            Some(("session".into(), "abc".into()))
        );
        assert_eq!(parse_set_cookie("a=\"b=c\""), Some(("a".into(), "b=c".into())));
        assert_eq!(parse_set_cookie("nonsense"), None);
    }

    #[test]
    fn header_names_are_capitalised() {
        assert_eq!(header_name("content-type"), "Content-Type");
        assert_eq!(header_name("x-multi"), "X-Multi");
        assert_eq!(header_name("etag"), "Etag");
    }

    #[test]
    fn tls_is_verified_unless_insecure() {
        assert!(!agent(false, None).config().tls_config().disable_verification());
        assert!(agent(true, None).config().tls_config().disable_verification());
        assert!(matches!(
            agent(false, None).config().tls_config().root_certs(),
            RootCerts::PlatformVerifier
        ));
    }
}
