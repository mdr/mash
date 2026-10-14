use std::fmt;
use std::rc::Rc;

use mash_syntax::TextRange;

use crate::ast::SourceUnit;

/// A place in the source.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Location {
    pub unit: Rc<SourceUnit>,
    pub range: TextRange,
}

impl Location {
    pub fn snippet(&self) -> &str {
        self.unit.snippet(self.range)
    }
}

/// One entry in a Mash stack trace (Scala `StackTraceItem`): a location, and the function it is in.
#[derive(Clone, Debug)]
pub struct StackFrame {
    pub location: Option<Location>,
    pub function: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ErrorKind {
    /// An error during evaluation
    Evaluation,
    /// A problem with the arguments of a call (Scala `ArgumentException`): reported at the argument, or else
    /// at the call
    Argument,
    /// Evaluation was interrupted (Ctrl-C, or a step budget ran out)
    Interrupted,
}

/// An error raised while evaluating Mash code, with a Mash-level stack trace.
#[derive(Clone, Debug)]
pub struct MashError {
    pub message: String,
    pub kind: ErrorKind,
    /// Innermost first: where the error happened, then each call site on the way out. Each frame records the
    /// function its location is in, where known. Empty if the error had no location when raised (it is then
    /// reported at the call it happened in).
    pub stack: Vec<StackFrame>,
}

pub type R<T> = Result<T, MashError>;

impl MashError {
    /// An error with no location yet: the location of the call it happens in is added as it propagates
    pub fn new(message: impl Into<String>) -> Self {
        MashError {
            message: message.into(),
            kind: ErrorKind::Evaluation,
            stack: vec![],
        }
    }

    pub fn at(message: impl Into<String>, location: Option<Location>) -> Self {
        MashError {
            message: message.into(),
            kind: ErrorKind::Evaluation,
            stack: vec![StackFrame {
                location,
                function: None,
            }],
        }
    }

    /// A problem with the arguments of a call
    pub fn argument(message: impl Into<String>, location: Option<Location>) -> Self {
        MashError {
            kind: ErrorKind::Argument,
            ..MashError::at(message, location)
        }
    }

    pub fn interrupted() -> Self {
        MashError {
            message: "Interrupted".into(),
            kind: ErrorKind::Interrupted,
            stack: vec![],
        }
    }

    pub fn is_interrupted(&self) -> bool {
        self.kind == ErrorKind::Interrupted
    }

    /// Where the error happened (the innermost location known)
    pub fn location(&self) -> Option<&Location> {
        self.stack.iter().find_map(|f| f.location.as_ref())
    }

    /// Record that the error came from a call to `function` at `call_site` (Scala
    /// `addInvocationToStackOnException`)
    pub(crate) fn through_call(mut self, function: Option<String>, call_site: Option<Location>) -> Self {
        match self.kind {
            ErrorKind::Interrupted => self,
            ErrorKind::Argument => {
                // Reported at the argument if known, else at the call
                if self.location().is_none() {
                    self.stack = vec![StackFrame {
                        location: call_site,
                        function: None,
                    }];
                }
                self.kind = ErrorKind::Evaluation;
                self
            }
            ErrorKind::Evaluation => {
                // The outermost frame so far is in the function being returned from
                if let Some(outermost) = self.stack.last_mut() {
                    outermost.function = function;
                }
                self.stack.push(StackFrame {
                    location: call_site,
                    function: None,
                });
                self
            }
        }
    }

    /// Give the error a location if it has none (e.g. one raised by a builtin)
    pub(crate) fn or_at(mut self, location: Option<Location>) -> Self {
        if self.stack.is_empty() && self.kind != ErrorKind::Interrupted {
            self.stack.push(StackFrame {
                location,
                function: None,
            });
        }
        self
    }
}

impl fmt::Display for MashError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for MashError {}

/// ". Did you mean 'x'?" for the name closest to `requested`, if any is close enough: at most one edit per
/// three characters of `requested`, and at least one, ignoring case (as rustc's suggestions)
pub fn suggestion_suffix(names: &[String], requested: &str) -> String {
    let requested = requested.to_lowercase();
    let max_distance = requested.chars().count().max(3) / 3;
    names
        .iter()
        .map(|name| (edit_distance(&requested, &name.to_lowercase()), name))
        .filter(|(distance, _)| *distance <= max_distance)
        .min_by_key(|(distance, _)| *distance)
        .map(|(_, name)| format!(". Did you mean '{name}'?"))
        .unwrap_or_default()
}

/// Optimal string alignment distance: inserting, deleting or changing a character, or swapping two
/// neighbouring characters, each count as one edit
fn edit_distance(a: &str, b: &str) -> usize {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();
    let mut d = vec![vec![0; b.len() + 1]; a.len() + 1];
    for (i, row) in d.iter_mut().enumerate() {
        row[0] = i;
    }
    for (j, cell) in d[0].iter_mut().enumerate() {
        *cell = j;
    }
    for i in 1..=a.len() {
        for j in 1..=b.len() {
            let cost = usize::from(a[i - 1] != b[j - 1]);
            d[i][j] = (d[i - 1][j] + 1).min(d[i][j - 1] + 1).min(d[i - 1][j - 1] + cost);
            if i > 1 && j > 1 && a[i - 1] == b[j - 2] && a[i - 2] == b[j - 1] {
                d[i][j] = d[i][j].min(d[i - 2][j - 2] + 1);
            }
        }
    }
    d[a.len()][b.len()]
}

impl Location {
    /// 1-based line and column (in chars) of the start of the location
    pub fn line_col(&self) -> (usize, usize) {
        let start = usize::from(self.range.start());
        let before = &self.unit.text[..start];
        let line = before.matches('\n').count() + 1;
        let line_start = before.rfind('\n').map_or(0, |i| i + 1);
        (line, before[line_start..].chars().count() + 1)
    }
}

impl MashError {
    /// A multi-line report: the message, the offending source line with the location underlined, and the Mash
    /// stack trace.
    pub fn report(&self) -> String {
        let mut out = format!("error: {}", self.message);
        if let Some(location) = self.location() {
            let (line, col) = location.line_col();
            let start = usize::from(location.range.start());
            let text = &location.unit.text;
            let line_start = text[..start].rfind('\n').map_or(0, |i| i + 1);
            let line_end = text[start..].find('\n').map_or(text.len(), |i| start + i);
            let source_line = &text[line_start..line_end];
            let width = text[start..usize::from(location.range.end()).min(line_end)]
                .chars()
                .count()
                .max(1);
            out += &format!(
                "\n  --> {}:{line}:{col}\n   | {source_line}\n   | {}{}",
                location.unit.name,
                " ".repeat(col - 1),
                "^".repeat(width)
            );
        }
        let frames: Vec<String> = self
            .stack
            .iter()
            .skip(1)
            .filter_map(|frame| {
                let location = frame.location.as_ref()?;
                let (line, col) = location.line_col();
                let function = frame
                    .function
                    .as_deref()
                    .map(|f| format!(" in {f}"))
                    .unwrap_or_default();
                Some(format!("  called at {}:{line}:{col}{function}", location.unit.name))
            })
            .collect();
        if !frames.is_empty() {
            out += "\n";
            out += &frames.join("\n");
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn suggestions() {
        let names = ["foo".to_string(), "bar".to_string(), "length".to_string()];
        assert_eq!(suggestion_suffix(&names, "lenght"), ". Did you mean 'length'?");
        assert_eq!(suggestion_suffix(&names, "Fo"), ". Did you mean 'foo'?");
        assert_eq!(suggestion_suffix(&[], "x"), "");
        // Too far from anything
        assert_eq!(suggestion_suffix(&names, "keys"), "");
        assert_eq!(suggestion_suffix(&names, "zzz"), "");
        // A swap of neighbouring letters is one edit
        assert_eq!(suggestion_suffix(&names, "lnegth"), ". Did you mean 'length'?");
        assert_eq!(edit_distance("kitten", "sitting"), 3);
        assert_eq!(edit_distance("yaer", "year"), 1);
    }
}
