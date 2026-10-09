//! Types for the network operations of the [`Platform`](super::Platform): HTTP requests and their responses.

use base64::Engine;
use base64::engine::general_purpose::STANDARD;

/// An HTTP request (see [`Platform::http_request`](super::Platform::http_request)).
#[derive(Debug, Clone, Copy)]
pub struct HttpRequest<'a> {
    /// `GET`, `POST`, `PUT`, `PATCH` or `DELETE`
    pub method: &'a str,
    /// An absolute `http` or `https` URL, query parameters included
    pub url: &'a str,
    /// Headers to send, in order (a name may repeat)
    pub headers: &'a [(String, String)],
    /// The body, if the request has one
    pub body: Option<&'a [u8]>,
    /// Basic authentication: a username and password
    pub basic_auth: Option<(&'a str, &'a str)>,
    /// Cookies to send (names and values)
    pub cookies: &'a [(String, String)],
    /// Don't verify the server's TLS certificate or host name
    pub insecure: bool,
}

impl HttpRequest<'_> {
    /// Every header to send: [`HttpRequest::headers`], then `Cookie` for the cookies (`name=value`, joined
    /// with `;`, as Scala Mash sent them; unless a `Cookie` header is given, which takes precedence, as in
    /// Scala) and `Authorization` for basic authentication (replacing any `Authorization` header given). The
    /// username and password are encoded as ISO-8859-1, as by Apache's HTTP client (other characters become
    /// `?`).
    pub fn all_headers(&self) -> Vec<(String, String)> {
        let mut headers = self.headers.to_vec();
        let has_header = |headers: &[(String, String)], wanted: &str| {
            headers.iter().any(|(name, _)| name.eq_ignore_ascii_case(wanted))
        };
        if !self.cookies.is_empty() && !has_header(&headers, "cookie") {
            let cookies: Vec<String> = self
                .cookies
                .iter()
                .map(|(name, value)| format!("{name}={value}"))
                .collect();
            headers.push(("Cookie".into(), cookies.join(";")));
        }
        if let Some((username, password)) = self.basic_auth {
            let latin1: Vec<u8> = format!("{username}:{password}")
                .chars()
                .map(|c| u8::try_from(u32::from(c)).unwrap_or(b'?'))
                .collect();
            headers.retain(|(name, _)| !name.eq_ignore_ascii_case("authorization"));
            headers.push(("Authorization".into(), format!("Basic {}", STANDARD.encode(latin1))));
        }
        headers
    }
}

/// The response to an HTTP request. Any status counts as a response (4xx and 5xx aren't errors).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct HttpResponse {
    pub status: u16,
    /// Response headers, in order (a name may repeat)
    pub headers: Vec<(String, String)>,
    /// Cookies the server set during the exchange (including on redirects): names and values
    pub cookies: Vec<(String, String)>,
    pub body: Vec<u8>,
}

impl HttpResponse {
    /// A response with this status and body, and no headers or cookies
    pub fn new(status: u16, body: impl Into<Vec<u8>>) -> HttpResponse {
        HttpResponse {
            status,
            body: body.into(),
            ..HttpResponse::default()
        }
    }

    pub fn with_header(mut self, name: &str, value: &str) -> HttpResponse {
        self.headers.push((name.into(), value.into()));
        self
    }

    pub fn with_cookie(mut self, name: &str, value: &str) -> HttpResponse {
        self.cookies.push((name.into(), value.into()));
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cookies_and_basic_auth_become_headers() {
        let headers = [
            ("Authorization".to_string(), "Bearer x".to_string()),
            ("A".into(), "1".into()),
        ];
        let cookies = [("a".to_string(), "1".to_string()), ("b".into(), "2".into())];
        let request = HttpRequest {
            method: "GET",
            url: "http://example.com",
            headers: &headers,
            body: None,
            basic_auth: Some(("aladdin", "opensesame")),
            cookies: &cookies,
            insecure: false,
        };
        assert_eq!(
            request.all_headers(),
            vec![
                ("A".to_string(), "1".to_string()),
                ("Cookie".into(), "a=1;b=2".into()),
                ("Authorization".into(), "Basic YWxhZGRpbjpvcGVuc2VzYW1l".into()),
            ]
        );
        let cookie_header = [("cookie".to_string(), "c=3".to_string())];
        let request = HttpRequest {
            headers: &cookie_header,
            basic_auth: None,
            ..request
        };
        assert_eq!(request.all_headers(), cookie_header.to_vec());
        let request = HttpRequest {
            headers: &[],
            cookies: &[],
            basic_auth: Some(("é", "€")),
            ..request
        };
        assert_eq!(
            request.all_headers()[0].1,
            format!("Basic {}", STANDARD.encode(b"\xe9:?"))
        );
    }
}
