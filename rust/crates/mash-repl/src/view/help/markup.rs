//! Documentation text with `<mash>…</mash>` code blocks (Scala `MashMarkupRenderer`).

use crate::screen::StyledString;
use crate::view::highlight::highlight;

const OPEN: &str = "<mash>";
const CLOSE: &str = "</mash>";

/// `s` with the markup removed and the code inside `<mash>…</mash>` syntax-highlighted. A newline just
/// inside each tag is dropped.
pub fn render_markup(s: &str) -> StyledString {
    let mut result = StyledString::new();
    let mut rest = s;
    while let Some(open) = rest.find(OPEN) {
        let contents_start = open + OPEN.len();
        // The code is at least one character long
        let Some(first_char) = rest[contents_start..].chars().next() else {
            break;
        };
        let search_from = contents_start + first_char.len_utf8();
        let Some(close) = rest[search_from..].find(CLOSE).map(|i| i + search_from) else {
            break;
        };
        result.append(&StyledString::plain(&rest[..open]));
        result.append(&highlight(trim_newlines(&rest[contents_start..close])));
        rest = &rest[close + CLOSE.len()..];
    }
    result.append(&StyledString::plain(rest));
    result
}

fn trim_newlines(s: &str) -> &str {
    let s = s.strip_prefix('\r').unwrap_or(s);
    let s = s.strip_prefix('\n').unwrap_or(s);
    let s = s.strip_suffix('\n').unwrap_or(s);
    s.strip_suffix('\r').unwrap_or(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn removes_markup() {
        assert_eq!(
            render_markup("Examples:\n<mash>\n  x = 1\n</mash> and <mash>y</mash>.").forget_styling(),
            "Examples:\n  x = 1 and y."
        );
        assert_eq!(render_markup("no code <mash>").forget_styling(), "no code <mash>");
    }

    #[test]
    fn highlights_code() {
        let rendered = render_markup("a <mash>1</mash>");
        assert_eq!(rendered.chars[0].style, crate::screen::Style::DEFAULT);
        assert_ne!(rendered.chars[2].style, crate::screen::Style::DEFAULT);
    }
}
