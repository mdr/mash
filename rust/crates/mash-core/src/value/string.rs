use std::fmt;
use std::rc::Rc;

use super::Tag;

/// An immutable Mash string, with an optional tag class (e.g. `os.Path`, `core.Character`).
///
/// Mash-visible lengths and indexes count `char`s (Unicode scalar values). Whether the text is pure ASCII is
/// cached, so the common case of indexing is O(1).
#[derive(Clone)]
pub struct Str {
    text: Rc<str>,
    tag: Option<Tag>,
    ascii: bool,
}

impl Str {
    pub fn new(text: impl Into<Rc<str>>) -> Self {
        let text: Rc<str> = text.into();
        let ascii = text.is_ascii();
        Str { text, tag: None, ascii }
    }

    pub fn tagged(text: impl Into<Rc<str>>, tag: Tag) -> Self {
        Str {
            tag: Some(tag),
            ..Str::new(text)
        }
    }

    pub fn as_str(&self) -> &str {
        &self.text
    }

    pub fn rc(&self) -> &Rc<str> {
        &self.text
    }

    pub fn tag(&self) -> Option<Tag> {
        self.tag
    }

    pub fn with_tag(&self, tag: Option<Tag>) -> Str {
        Str { tag, ..self.clone() }
    }

    /// Different text, same tag
    pub fn with_text(&self, text: impl Into<Rc<str>>) -> Str {
        Str {
            tag: self.tag,
            ..Str::new(text)
        }
    }

    /// Length in chars
    pub fn len(&self) -> usize {
        if self.ascii {
            self.text.len()
        } else {
            self.text.chars().count()
        }
    }

    pub fn is_empty(&self) -> bool {
        self.text.is_empty()
    }

    /// Resolve a Mash index (negative counts from the end) to a char index, if in range.
    pub fn resolve_index(&self, index: i64) -> Option<usize> {
        let len = self.len() as i64;
        let i = if index < 0 { index + len } else { index };
        (0..len).contains(&i).then_some(i as usize)
    }

    /// The char at `index` (negative counts from the end)
    pub fn char_at(&self, index: i64) -> Option<char> {
        let i = self.resolve_index(index)?;
        if self.ascii {
            Some(self.text.as_bytes()[i] as char)
        } else {
            self.text.chars().nth(i)
        }
    }

    /// The text between two char indexes (clamped to the string)
    pub fn char_slice(&self, start: usize, end: usize) -> &str {
        if self.ascii {
            let end = end.min(self.text.len());
            &self.text[start.min(end)..end]
        } else {
            let byte_offset = |i: usize| self.text.char_indices().nth(i).map_or(self.text.len(), |(b, _)| b);
            let (start, end) = (byte_offset(start), byte_offset(end));
            &self.text[start.min(end)..end]
        }
    }
}

impl From<&str> for Str {
    fn from(s: &str) -> Self {
        Str::new(s)
    }
}

impl From<String> for Str {
    fn from(s: String) -> Self {
        Str::new(s)
    }
}

impl fmt::Debug for Str {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&*self.text, f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn indexing_counts_chars() {
        for s in [Str::new("héllo"), Str::new("hello")] {
            assert_eq!(s.len(), 5);
            assert_eq!(s.char_at(1).map(|c| c.is_alphabetic()), Some(true));
            assert_eq!(s.char_at(-1), Some('o'));
            assert_eq!(s.char_at(5), None);
            assert_eq!(s.char_at(-6), None);
            assert_eq!(s.char_slice(1, 3).chars().count(), 2);
            assert_eq!(s.char_slice(3, 99), "lo");
            assert_eq!(s.char_slice(4, 2), "");
        }
        assert_eq!(Str::new("😀x").len(), 2);
        assert_eq!(Str::new("😀x").char_at(1), Some('x'));
    }
}
