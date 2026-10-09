//! Editor actions, and the key maps that bind keys to them in each mode (Scala `NormalActions`,
//! `IncrementalHistorySearchActions`, `BrowseCompletionActions` and the `input` key maps).

use crate::input::{Key, KeyPress};

/// Something the user asked the editor to do.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Action {
    /// Insert text (a typed character, or text pasted in one go)
    SelfInsert(String),
    /// Clear the terminal and redraw (Ctrl-L)
    RedrawScreen,
    /// Run the command, or start a new line if it's incomplete
    Enter,
    /// Insert a line break (Alt-Enter)
    Newline,
    BeginningOfLine,
    BeginningOfLineExtendingSelection,
    EndOfLine,
    EndOfLineExtendingSelection,
    ForwardChar,
    BackwardChar,
    ForwardCharExtendingSelection,
    BackwardCharExtendingSelection,
    ForwardWord,
    BackwardWord,
    ForwardWordExtendingSelection,
    BackwardWordExtendingSelection,
    /// Previous line, or previous history entry
    Up,
    UpExtendingSelection,
    Down,
    DownExtendingSelection,
    DeleteChar,
    /// Ctrl-D on an empty line: leave the REPL
    EndOfFile,
    BackwardDeleteChar,
    KillLine,
    BackwardKillLine,
    KillWord,
    /// Delete the previous word, or cut the selection
    BackwardKillWord,
    BeginningOfBuffer,
    EndOfBuffer,
    Copy,
    Paste,
    Undo,
    Redo,
    Complete,
    BackwardComplete,
    Noop,
    /// Toggle the calling syntax of the function being invoked (Ctrl-Space)
    AssistInvocation,
    IncrementalHistorySearch,
    /// Abandon the line (or close the assistance box, leave a mode)
    Quit,
    PageUp,
    PageDown,
    /// Insert the last argument of a previous command (Alt-.); repeat to go further back
    InsertLastArg,
    ToggleQuote,
    ExpandSelection,
    UnexpandSelection,
    /// Toggle the `!` that runs a line as mish (Alt-,)
    ToggleMish,
    /// Browse the last result in the object browser (Ctrl-V; not yet available)
    BrowseLastResult,
    /// Evaluate the line (or selection) and replace it with the result (Alt-E)
    Inline,

    // Incremental history search
    /// Change to the directory the matching command was run in
    ChangeDirectory,
    ToggleCurrentDirOnly,
    FirstHit,
    LastHit,

    // Browsing completions
    NextCompletion,
    PreviousCompletion,
    NavigateDown,
    NavigateUp,
    NavigateLeft,
    NavigateRight,
    AcceptCompletion,
}

/// Which key map applies.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum KeyMap {
    Normal,
    IncrementalHistorySearch,
    BrowseCompletions,
}

impl KeyMap {
    /// The action for a key press. Keys the map doesn't bind fall back to the normal map, and then printable
    /// characters insert themselves. `line_empty` decides whether Ctrl-D means end-of-file.
    pub fn action(self, key: KeyPress, line_empty: bool) -> Action {
        if self == KeyMap::Normal && line_empty && key == KeyPress::ctrl('d') {
            return Action::EndOfFile;
        }
        let specific = match self {
            KeyMap::Normal => None,
            KeyMap::IncrementalHistorySearch => history_search_action(key),
            KeyMap::BrowseCompletions => browse_completions_action(key),
        };
        specific.or_else(|| normal_action(key)).unwrap_or_else(|| match key {
            KeyPress {
                key: Key::Char(c),
                ctrl: false,
                alt: false,
                ..
            } if !c.is_control() => Action::SelfInsert(c.to_string()),
            _ => Action::Noop,
        })
    }
}

/// Scala `NormalKeyMap` (including `LineBufferKeyMap`)
fn normal_action(key: KeyPress) -> Option<Action> {
    use Action::*;
    let KeyPress { key, ctrl, alt, shift } = key;
    Some(match (key, ctrl, alt, shift) {
        (Key::Enter, false, true, _) => Newline,
        (Key::Enter, false, false, _) => Enter,
        (Key::Tab, false, false, false) => Complete,
        (Key::Tab, false, false, true) => BackwardComplete,
        (Key::Home, false, false, false) => BeginningOfLine,
        (Key::End, false, false, false) => EndOfLine,
        (Key::Home, false, false, true) => BeginningOfLineExtendingSelection,
        (Key::End, false, false, true) => EndOfLineExtendingSelection,
        (Key::Right, false, false, false) => ForwardChar,
        (Key::Left, false, false, false) => BackwardChar,
        (Key::Right, false, false, true) => ForwardCharExtendingSelection,
        (Key::Left, false, false, true) => BackwardCharExtendingSelection,
        (Key::Down, false, false, true) => DownExtendingSelection,
        (Key::Up, false, false, true) => UpExtendingSelection,
        (Key::Up, false, false, false) => Up,
        (Key::Down, false, false, false) => Down,
        (Key::Right, false, true, true) => ForwardWordExtendingSelection,
        (Key::Left, false, true, true) => BackwardWordExtendingSelection,
        // Not bound in Scala Mash (its escape-sequence reader recognised Ctrl-Left/Right but bound nothing)
        (Key::Right, true, false, false) => ForwardWord,
        (Key::Left, true, false, false) => BackwardWord,
        (Key::Up, false, true, false) => ExpandSelection,
        (Key::Down, false, true, false) => UnexpandSelection,
        (Key::Delete, false, false, _) => DeleteChar,
        (Key::Backspace, false, false, _) => BackwardDeleteChar,
        (Key::Backspace, false, true, _) => BackwardKillWord,
        (Key::PageUp, false, false, _) => PageUp,
        (Key::PageDown, false, false, _) => PageDown,
        (Key::Char(c), true, false, _) => match c.to_ascii_lowercase() {
            'l' => RedrawScreen,
            'a' => BeginningOfLine,
            'e' => EndOfLine,
            'f' => ForwardChar,
            'b' => BackwardChar,
            'd' => DeleteChar,
            'k' => KillLine,
            'u' => BackwardKillLine,
            'w' => BackwardKillWord,
            'q' => ToggleQuote,
            // Ctrl-C at the prompt abandons the line, like Ctrl-G (in Scala Mash it was a signal)
            'g' | 'c' => Quit,
            'y' => Paste,
            'p' => Up,
            'n' => Down,
            'r' => IncrementalHistorySearch,
            'v' => BrowseLastResult,
            // Ctrl-_ sends 0x1F, which terminals also send for Ctrl-/ and Ctrl-7
            '_' | '/' | '7' => Undo,
            // Ctrl-Space sends NUL (Ctrl-@)
            ' ' | '@' | '2' => AssistInvocation,
            _ => return None,
        },
        (Key::Char(c), false, true, false) => match c {
            'e' => Inline,
            'f' => ForwardWord,
            'b' => BackwardWord,
            // Alt-Shift-F and Alt-Shift-B, as most terminals send them
            'F' => ForwardWordExtendingSelection,
            'B' => BackwardWordExtendingSelection,
            'd' => KillWord,
            '<' => BeginningOfBuffer,
            '>' => EndOfBuffer,
            'w' => Copy,
            '.' => InsertLastArg,
            ',' => ToggleMish,
            '_' => Redo,
            _ => return None,
        },
        (Key::Char(c), false, true, true) => match c.to_ascii_lowercase() {
            'f' => ForwardWordExtendingSelection,
            'b' => BackwardWordExtendingSelection,
            '_' => Redo,
            '<' => BeginningOfBuffer,
            '>' => EndOfBuffer,
            _ => return None,
        },
        _ => return None,
    })
}

/// Scala `IncrementalHistorySearchKeyMap`
fn history_search_action(key: KeyPress) -> Option<Action> {
    use Action::*;
    Some(match (key.key, key.ctrl, key.alt) {
        (Key::Char('d'), true, false) => ChangeDirectory,
        (Key::Char('d'), false, true) => ToggleCurrentDirOnly,
        (Key::Home, false, false) | (Key::Char('<'), false, true) => LastHit,
        (Key::End, false, false) | (Key::Char('>'), false, true) => FirstHit,
        _ => return None,
    })
}

/// Scala `BrowseCompletionsKeyMap`
fn browse_completions_action(key: KeyPress) -> Option<Action> {
    use Action::*;
    Some(match (key.key, key.ctrl, key.alt, key.shift) {
        (Key::Tab, false, false, false) => NextCompletion,
        (Key::Tab, false, false, true) => PreviousCompletion,
        (Key::Right, false, false, false) => NavigateRight,
        (Key::Left, false, false, false) | (Key::Char('b'), true, false, _) => NavigateLeft,
        (Key::Up, false, false, false) | (Key::Char('p'), true, false, _) => NavigateUp,
        (Key::Down, false, false, false) | (Key::Char('n'), true, false, _) => NavigateDown,
        (Key::Enter, false, false, _) => AcceptCompletion,
        _ => return None,
    })
}

/// A short description of a key press, with symbols (Scala `KeyDescriber`): `⌃g`, `⌥d`, `↩`
pub fn describe_key(key: KeyPress) -> String {
    if key == KeyPress::shift(Key::Tab) {
        return "⇤".into();
    }
    let mut s = String::new();
    if key.shift {
        s.push('⇧');
    }
    if key.ctrl {
        s.push('⌃');
    }
    if key.alt {
        s.push('⌥');
    }
    match key.key {
        Key::Char(' ') => s.push('␣'),
        Key::Char(c) => s.push(c),
        Key::Enter => s.push('↩'),
        Key::Backspace => s.push('⌫'),
        Key::Delete => s.push('␡'),
        Key::PageUp => s.push('⇞'),
        Key::PageDown => s.push('⇟'),
        Key::Home => s.push('↖'),
        Key::End => s.push('↘'),
        Key::Escape => s.push('⎋'),
        Key::Up => s.push('↑'),
        Key::Down => s.push('↓'),
        Key::Left => s.push('←'),
        Key::Right => s.push('→'),
        Key::Tab => s.push('⇥'),
        Key::F(n) => s.push_str(&format!("F{n}")),
    }
    s
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn printable_keys_insert_themselves() {
        let a = KeyPress::plain(Key::Char('a'));
        assert_eq!(KeyMap::Normal.action(a, true), Action::SelfInsert("a".into()));
        assert_eq!(KeyMap::Normal.action(KeyPress::ctrl('d'), true), Action::EndOfFile);
        assert_eq!(KeyMap::Normal.action(KeyPress::ctrl('d'), false), Action::DeleteChar);
        assert_eq!(KeyMap::Normal.action(KeyPress::ctrl('z'), false), Action::Noop);
    }

    #[test]
    fn modes_fall_back_to_normal_keys() {
        assert_eq!(
            KeyMap::IncrementalHistorySearch.action(KeyPress::ctrl('a'), false),
            Action::BeginningOfLine
        );
        assert_eq!(
            KeyMap::IncrementalHistorySearch.action(KeyPress::ctrl('d'), true),
            Action::ChangeDirectory
        );
        assert_eq!(
            KeyMap::BrowseCompletions.action(KeyPress::plain(Key::Tab), false),
            Action::NextCompletion
        );
    }

    #[test]
    fn key_descriptions() {
        assert_eq!(describe_key(KeyPress::ctrl('g')), "⌃g");
        assert_eq!(describe_key(KeyPress::alt(Key::Char('d'))), "⌥d");
        assert_eq!(describe_key(KeyPress::shift(Key::Tab)), "⇤");
    }
}
