//! Fitting text into columns (Scala `StringUtils` and `StyledStringUtils`). Widths are display columns, so
//! wide characters line up.

use unicode_width::UnicodeWidthStr;

use crate::screen::{StyledChar, StyledString};

pub(crate) const ELLIPSIS: char = '…';

pub(crate) fn width(s: &str) -> usize {
    s.width()
}

/// `s` cut down to at most `max_width` columns, ending with `…` if anything was cut
pub(crate) fn ellipsisise(s: &str, max_width: usize) -> String {
    if width(s) <= max_width {
        return s.to_string();
    }
    if max_width == 0 {
        return String::new();
    }
    let mut result = String::new();
    let mut used = 0;
    for c in s.chars() {
        let w = unicode_width::UnicodeWidthChar::width(c).unwrap_or(0);
        if used + w > max_width - 1 {
            break;
        }
        used += w;
        result.push(c);
    }
    result.push(ELLIPSIS);
    result
}

/// `s` padded with spaces or cut down (with `…`) to exactly `columns` columns
pub(crate) fn fit_to_width(s: &str, columns: usize) -> String {
    let mut fit = ellipsisise(s, columns);
    let w = width(&fit);
    fit.extend(std::iter::repeat_n(' ', columns.saturating_sub(w)));
    fit
}

/// `s` centred in `columns` columns (extra space goes on the right)
pub(crate) fn centre(s: &str, columns: usize) -> String {
    let gap = columns.saturating_sub(width(s));
    format!("{}{s}{}", " ".repeat(gap / 2), " ".repeat(gap - gap / 2))
}

/// `s` cut down to at most `max_width` columns, ending with `ellipsis` if anything was cut
pub(crate) fn ellipsisise_styled(s: &StyledString, max_width: usize, ellipsis: StyledChar) -> StyledString {
    if s.width() <= max_width {
        return s.clone();
    }
    let mut result = StyledString::new();
    if max_width == 0 {
        return result;
    }
    let mut used = 0;
    for c in &s.chars {
        if used + c.width() > max_width - 1 {
            break;
        }
        used += c.width();
        result.push(*c);
    }
    result.push(ellipsis);
    result
}

/// Split into lines on `\n` (and `\r\n`)
pub(crate) fn lines(s: &str) -> impl Iterator<Item = &str> {
    s.split('\n').map(|line| line.strip_suffix('\r').unwrap_or(line))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fitting() {
        assert_eq!(fit_to_width("abc", 5), "abc  ");
        assert_eq!(fit_to_width("abcdef", 4), "abc…");
        assert_eq!(fit_to_width("abc", 0), "");
        assert_eq!(fit_to_width("日本語", 4), "日… ");
        assert_eq!(ellipsisise("abc", 1), "…");
    }

    #[test]
    fn centring() {
        assert_eq!(centre("ab", 5), " ab  ");
        assert_eq!(centre("abc", 2), "abc");
    }
}
