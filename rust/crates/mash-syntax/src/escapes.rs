//! Backtick string escapes (port of `StringEscapes`). Mash uses `` ` `` as its escape character:
//! `` `n `` is a newline, `` `$ `` a literal dollar, and so on.

/// Undo backtick escapes. `` `n ``, `` `r ``, `` `t `` are control characters; a backtick before any other
/// character yields that character; a trailing lone backtick is kept.
pub fn unescape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '`' {
            match chars.next() {
                Some('n') => out.push('\n'),
                Some('r') => out.push('\r'),
                Some('t') => out.push('\t'),
                Some(c) => out.push(c),
                None => out.push('`'),
            }
        } else {
            out.push(c);
        }
    }
    out
}

/// Escape a string for inclusion in a double-quoted Mash string literal.
pub fn escape_chars(s: &str) -> String {
    escape_chars_full(s).escaped
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EscapeResult {
    pub escaped: String,
    /// For each char of the original string, the char index at which it starts in `escaped`. Used to map a
    /// cursor position in a value onto the corresponding position in its literal.
    pub index_map: Vec<usize>,
}

pub fn escape_chars_full(s: &str) -> EscapeResult {
    let mut escaped = String::with_capacity(s.len());
    let mut index_map = Vec::with_capacity(s.len());
    let mut escaped_len = 0;
    for c in s.chars() {
        index_map.push(escaped_len);
        let escape = match c {
            '$' => Some('$'),
            '"' => Some('"'),
            '`' => Some('`'),
            '\r' => Some('r'),
            '\n' => Some('n'),
            '\t' => Some('t'),
            _ => None,
        };
        match escape {
            Some(e) => {
                escaped.push('`');
                escaped.push(e);
                escaped_len += 2;
            }
            None => {
                escaped.push(c);
                escaped_len += 1;
            }
        }
    }
    EscapeResult { escaped, index_map }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unescape_works() {
        // From StringEscapesTest.scala
        assert_eq!(unescape(""), "");
        assert_eq!(unescape("a"), "a");
        assert_eq!(unescape("`n"), "\n");
        assert_eq!(unescape("`t"), "\t");
        assert_eq!(unescape("`r"), "\r");
        assert_eq!(unescape("`~"), "~");
        assert_eq!(unescape("`$"), "$");
        assert_eq!(unescape("`'"), "'");
        assert_eq!(unescape(" `\" "), " \" ");
        assert_eq!(unescape("`"), "`");
        assert_eq!(unescape("`a"), "a");
    }

    #[test]
    fn escape_round_trips() {
        for s in [
            "",
            "plain",
            "a$b",
            "quote\"d",
            "back`tick",
            "line\nbreak\r\ttab",
            "ünïcödé $x",
        ] {
            assert_eq!(unescape(&escape_chars(s)), s);
        }
    }

    #[test]
    fn index_map() {
        let result = escape_chars_full("a$b");
        assert_eq!(result.escaped, "a`$b");
        assert_eq!(result.index_map, [0, 1, 3]);
    }
}
