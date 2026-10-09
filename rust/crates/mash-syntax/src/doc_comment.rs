//! Doc comments on `def` and `class` declarations: the `#` comment lines immediately before them.
//! Port of `DocCommentParser`.
//!
//! ```text
//! # Summary line
//! # Further description,
//! # possibly several lines
//! # @param n The number to square
//! def square n = n * n
//! ```

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocComment {
    pub summary: String,
    pub description: Option<String>,
    pub params: Vec<ParamComment>,
}

/// An `@param name summary` line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParamComment {
    pub name: String,
    pub summary: String,
}

impl DocComment {
    pub fn param(&self, name: &str) -> Option<&ParamComment> {
        self.params.iter().find(|p| p.name == name)
    }
}

/// Parse the text of a doc comment: the comment lines with their leading `#` removed, joined by newlines.
/// The first line is the summary; `@param` lines are extracted; anything else is the description.
pub fn parse(text: &str) -> DocComment {
    let lines: Vec<&str> = text
        .lines()
        .map(|line| line.strip_prefix(' ').unwrap_or(line))
        .collect();
    let (param_lines, main_lines): (Vec<&str>, Vec<&str>) = lines.into_iter().partition(|line| is_param_line(line));
    let mut main_lines = main_lines.into_iter();
    let summary = main_lines.next().unwrap_or_default().to_string();
    let rest: Vec<&str> = main_lines.collect();
    let description = if rest.iter().all(|line| line.trim().is_empty()) {
        None
    } else {
        Some(rest.join("\n"))
    };
    let params = param_lines.into_iter().filter_map(parse_param_line).collect();
    DocComment {
        summary,
        description,
        params,
    }
}

fn is_param_line(line: &str) -> bool {
    line.trim_start()
        .strip_prefix("@param")
        .is_some_and(|rest| !rest.is_empty())
}

/// `@param name summary` (whitespace-separated)
fn parse_param_line(line: &str) -> Option<ParamComment> {
    let rest = line.trim_start().strip_prefix("@param")?;
    if !rest.starts_with(char::is_whitespace) {
        return None;
    }
    let rest = rest.trim_start();
    let name_end = rest.find(|c: char| !(c.is_alphanumeric() || c == '_'))?;
    let (name, after) = rest.split_at(name_end);
    if name.is_empty() || !after.starts_with(char::is_whitespace) {
        return None;
    }
    Some(ParamComment {
        name: name.to_string(),
        summary: after.trim_start().to_string(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn summary_only() {
        let doc = parse(" Square a number\n");
        assert_eq!(doc.summary, "Square a number");
        assert_eq!(doc.description, None);
    }

    #[test]
    fn summary_and_description() {
        let doc = parse(" Summary\n Description 1\n Description 2\n Description 3\n");
        assert_eq!(doc.summary, "Summary");
        assert_eq!(
            doc.description.as_deref(),
            Some("Description 1\nDescription 2\nDescription 3")
        );
    }

    #[test]
    fn blank_description_is_none() {
        assert_eq!(parse(" Summary\n \n").description, None);
    }

    #[test]
    fn only_one_initial_space_is_dropped() {
        let doc = parse(" Summary\n   indented\n");
        assert_eq!(doc.description.as_deref(), Some("  indented"));
    }

    #[test]
    fn param_comments() {
        let doc = parse(" Add numbers\n @param x The first\n More text\n @param y The second\n");
        assert_eq!(doc.summary, "Add numbers");
        assert_eq!(doc.description.as_deref(), Some("More text"));
        assert_eq!(doc.param("x").map(|p| p.summary.as_str()), Some("The first"));
        assert_eq!(doc.param("y").map(|p| p.summary.as_str()), Some("The second"));
    }

    #[test]
    fn empty() {
        assert_eq!(
            parse(""),
            DocComment {
                summary: String::new(),
                description: None,
                params: vec![]
            }
        );
    }
}
