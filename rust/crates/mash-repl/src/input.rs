//! Input events, independent of any terminal library: the host (crossterm natively, xterm.js in the browser)
//! translates its own events into these.

use crate::screen::Dimensions;

/// A key, without modifiers. Printable keys (including space) are `Char`.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Key {
    Char(char),
    Enter,
    Tab,
    Backspace,
    Delete,
    Escape,
    Up,
    Down,
    Left,
    Right,
    Home,
    End,
    PageUp,
    PageDown,
    /// A function key, F1 to F12
    F(u8),
}

/// A key with modifiers. `Char` keys are given as typed: Shift+a is `Char('A')` with `shift` unset, and
/// Ctrl+a is `Char('a')` with `ctrl` set.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct KeyPress {
    pub key: Key,
    pub ctrl: bool,
    pub alt: bool,
    pub shift: bool,
}

impl KeyPress {
    pub fn plain(key: Key) -> Self {
        KeyPress {
            key,
            ctrl: false,
            alt: false,
            shift: false,
        }
    }

    pub fn ctrl(c: char) -> Self {
        KeyPress {
            ctrl: true,
            ..KeyPress::plain(Key::Char(c))
        }
    }

    pub fn alt(key: Key) -> Self {
        KeyPress {
            alt: true,
            ..KeyPress::plain(key)
        }
    }

    pub fn shift(key: Key) -> Self {
        KeyPress {
            shift: true,
            ..KeyPress::plain(key)
        }
    }

    pub fn alt_shift(key: Key) -> Self {
        KeyPress {
            alt: true,
            shift: true,
            ..KeyPress::plain(key)
        }
    }
}

/// Something that happened at the terminal.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum InputEvent {
    Key(KeyPress),
    /// Text pasted in one go (bracketed paste): inserted as is, never treated as keys
    Paste(String),
    /// The terminal changed size
    Resize(Dimensions),
}
