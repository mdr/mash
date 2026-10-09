//! Incremental search in the object browser's tables (Scala `SearchState`): the query is a regular
//! expression (or, if it isn't a valid one, literal text), matched against each cell as it's displayed.

use std::collections::BTreeMap;
use std::ops::Range;

use fancy_regex::{Regex, RegexBuilder};

/// A search in progress, and where it matches.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct SearchState {
    pub query: String,
    pub ignore_case: bool,
    /// The matches (as character ranges) in each cell that has any, by row and column
    matches: BTreeMap<(usize, usize), Vec<Range<usize>>>,
}

impl SearchState {
    /// A search for `query` in each cell of `cells` (row, column and text)
    pub(crate) fn new<'a>(
        query: &str,
        ignore_case: bool,
        cells: impl IntoIterator<Item = (usize, usize, &'a str)>,
    ) -> SearchState {
        let regex = compile(query, ignore_case);
        let matches = cells
            .into_iter()
            .filter_map(|(row, column, text)| {
                let found = find_matches(&regex, text);
                (!found.is_empty()).then_some(((row, column), found))
            })
            .collect();
        SearchState {
            query: query.to_string(),
            ignore_case,
            matches,
        }
    }

    /// A search that has just begun (nothing typed yet)
    pub(crate) fn empty() -> SearchState {
        SearchState {
            query: String::new(),
            ignore_case: true,
            matches: BTreeMap::new(),
        }
    }

    /// The rows with a match, in order
    pub fn rows(&self) -> Vec<usize> {
        let mut rows: Vec<usize> = self.matches.keys().map(|&(row, _)| row).collect();
        rows.dedup();
        rows
    }

    /// The matches in a cell, as character ranges
    pub fn cell_matches(&self, row: usize, column: usize) -> &[Range<usize>] {
        self.matches.get(&(row, column)).map_or(&[], Vec::as_slice)
    }

    /// The first row with a match at or after `row`, else the first with a match
    pub(crate) fn first_hit_from(&self, row: usize) -> Option<usize> {
        let rows = self.rows();
        rows.iter().find(|&&r| r >= row).or(rows.first()).copied()
    }

    /// The row with the next match after `row`, wrapping round
    pub(crate) fn next_hit(&self, row: usize) -> Option<usize> {
        let rows = self.rows();
        rows.iter().find(|&&r| r > row).or(rows.first()).copied()
    }

    /// The row with the previous match before `row`, wrapping round
    pub(crate) fn previous_hit(&self, row: usize) -> Option<usize> {
        let rows = self.rows();
        rows.iter().rev().find(|&&r| r < row).or(rows.last()).copied()
    }
}

/// The query as a regular expression, or as literal text if it isn't valid
fn compile(query: &str, ignore_case: bool) -> Regex {
    RegexBuilder::new(query)
        .case_insensitive(ignore_case)
        .build()
        .or_else(|_| {
            RegexBuilder::new(&fancy_regex::escape(query))
                .case_insensitive(ignore_case)
                .build()
        })
        .expect("an escaped query is a valid regular expression")
}

/// Each match of `regex` in `text`, as a range of characters (an empty query matches an empty range at the
/// start of each character, as in Java)
fn find_matches(regex: &Regex, text: &str) -> Vec<Range<usize>> {
    let char_index = |byte: usize| text[..byte].chars().count();
    regex
        .find_iter(text)
        .map_while(Result::ok)
        .map(|m| char_index(m.start())..char_index(m.end()))
        .collect()
}

#[cfg(test)]
#[allow(clippy::single_range_in_vec_init)]
mod tests {
    use super::*;

    #[test]
    fn matches_are_character_ranges() {
        let search = SearchState::new("b+", true, [(0, 0, "äbb"), (0, 1, "x"), (2, 0, "B")]);
        assert_eq!(search.cell_matches(0, 0), &[1..3]);
        assert_eq!(search.cell_matches(0, 1), &[] as &[Range<usize>]);
        assert_eq!(search.rows(), vec![0, 2]);
        assert_eq!(search.next_hit(0), Some(2));
        assert_eq!(search.next_hit(2), Some(0));
        assert_eq!(search.previous_hit(0), Some(2));
    }

    #[test]
    fn invalid_regexes_are_literal_text() {
        let search = SearchState::new("a(", false, [(0, 0, "xa(y"), (1, 0, "A(")]);
        assert_eq!(search.cell_matches(0, 0), &[1..3]);
        assert_eq!(search.rows(), vec![0]);
    }
}
