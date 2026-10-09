//! Browser keyboard events (`KeyboardEvent.key`, `.code` and modifiers) as the REPL's key presses.

use mash_repl::input::{Key, KeyPress};

/// A `keydown` event as the REPL's key press; `None` for keys the REPL doesn't use, and for anything with the
/// Meta (Command) key, which belongs to the browser (copy, paste, …).
///
/// `key` is what the key types given the layout and modifiers; `code` is the physical key. On macOS, Option
/// changes what a key types (Option-F types `ƒ`), so with Alt a non-ASCII `key` falls back to the physical
/// key's character on a US layout.
pub fn translate_key(key: &str, code: &str, ctrl: bool, alt: bool, shift: bool, meta: bool) -> Option<KeyPress> {
    if meta {
        return None;
    }
    let named = match key {
        "Enter" => Some(Key::Enter),
        "Tab" => Some(Key::Tab),
        "Backspace" => Some(Key::Backspace),
        "Delete" => Some(Key::Delete),
        "Escape" => Some(Key::Escape),
        "ArrowUp" => Some(Key::Up),
        "ArrowDown" => Some(Key::Down),
        "ArrowLeft" => Some(Key::Left),
        "ArrowRight" => Some(Key::Right),
        "Home" => Some(Key::Home),
        "End" => Some(Key::End),
        "PageUp" => Some(Key::PageUp),
        "PageDown" => Some(Key::PageDown),
        _ => key
            .strip_prefix('F')
            .and_then(|n| n.parse::<u8>().ok())
            .filter(|n| (1..=12).contains(n))
            .map(Key::F),
    };
    if let Some(key) = named {
        return Some(KeyPress { key, ctrl, alt, shift });
    }
    let mut chars = key.chars();
    let (Some(c), None) = (chars.next(), chars.next()) else {
        // Modifier keys on their own, dead keys, IME composition, …
        return None;
    };
    // AltGr (on Windows, Ctrl+Alt) types characters such as `@` on some layouts
    if ctrl && alt && !c.is_ascii_alphanumeric() && !c.is_control() {
        return Some(KeyPress::plain(Key::Char(c)));
    }
    let c = if alt && !c.is_ascii() {
        us_layout_char(code, shift)?
    } else if ctrl {
        c.to_ascii_lowercase()
    } else {
        c
    };
    // Shift is already reflected in the character
    Some(KeyPress {
        key: Key::Char(c),
        ctrl,
        alt,
        shift: false,
    })
}

/// The character a physical key types on a US keyboard
fn us_layout_char(code: &str, shift: bool) -> Option<char> {
    if let Some(letter) = code.strip_prefix("Key").filter(|l| l.len() == 1) {
        let c = letter.chars().next()?;
        return Some(if shift { c } else { c.to_ascii_lowercase() });
    }
    if let Some(digit) = code.strip_prefix("Digit").filter(|d| d.len() == 1) {
        let c = digit.chars().next()?;
        return Some(if shift {
            ")!@#$%^&*(".chars().nth(c.to_digit(10)? as usize)?
        } else {
            c
        });
    }
    let (plain, shifted) = match code {
        "Minus" => ('-', '_'),
        "Equal" => ('=', '+'),
        "BracketLeft" => ('[', '{'),
        "BracketRight" => (']', '}'),
        "Backslash" => ('\\', '|'),
        "Semicolon" => (';', ':'),
        "Quote" => ('\'', '"'),
        "Backquote" => ('`', '~'),
        "Comma" => (',', '<'),
        "Period" => ('.', '>'),
        "Slash" => ('/', '?'),
        "Space" => (' ', ' '),
        _ => return None,
    };
    Some(if shift { shifted } else { plain })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn key(key: &str, code: &str) -> Option<KeyPress> {
        translate_key(key, code, false, false, false, false)
    }

    #[test]
    fn named_keys() {
        assert_eq!(key("Enter", "Enter"), Some(KeyPress::plain(Key::Enter)));
        assert_eq!(key("ArrowLeft", "ArrowLeft"), Some(KeyPress::plain(Key::Left)));
        assert_eq!(key("F5", "F5"), Some(KeyPress::plain(Key::F(5))));
        assert_eq!(
            translate_key("Tab", "Tab", false, false, true, false),
            Some(KeyPress::shift(Key::Tab))
        );
        assert_eq!(
            translate_key("ArrowRight", "ArrowRight", false, true, false, false),
            Some(KeyPress::alt(Key::Right))
        );
    }

    #[test]
    fn characters_carry_shift_themselves() {
        assert_eq!(key("a", "KeyA"), Some(KeyPress::plain(Key::Char('a'))));
        assert_eq!(
            translate_key("A", "KeyA", false, false, true, false),
            Some(KeyPress::plain(Key::Char('A')))
        );
        assert_eq!(key(" ", "Space"), Some(KeyPress::plain(Key::Char(' '))));
        assert_eq!(key("é", "KeyE"), Some(KeyPress::plain(Key::Char('é'))));
    }

    #[test]
    fn control_keys_are_lower_case() {
        assert_eq!(
            translate_key("C", "KeyC", true, false, true, false),
            Some(KeyPress::ctrl('c'))
        );
        assert_eq!(
            translate_key("_", "Minus", true, false, true, false),
            Some(KeyPress::ctrl('_'))
        );
    }

    #[test]
    fn option_on_macos_means_the_physical_key() {
        assert_eq!(
            translate_key("ƒ", "KeyF", false, true, false, false),
            Some(KeyPress::alt(Key::Char('f')))
        );
        assert_eq!(
            translate_key("Ï", "KeyF", false, true, true, false),
            Some(KeyPress::alt(Key::Char('F')))
        );
        assert_eq!(
            translate_key("≥", "Period", false, true, false, false),
            Some(KeyPress::alt(Key::Char('.')))
        );
        // Elsewhere Alt doesn't change the character
        assert_eq!(
            translate_key("f", "KeyF", false, true, false, false),
            Some(KeyPress::alt(Key::Char('f')))
        );
    }

    #[test]
    fn altgr_types_characters() {
        assert_eq!(
            translate_key("@", "KeyQ", true, true, false, false),
            Some(KeyPress::plain(Key::Char('@')))
        );
    }

    #[test]
    fn the_browser_keeps_some_keys() {
        assert_eq!(translate_key("v", "KeyV", false, false, false, true), None);
        assert_eq!(key("Shift", "ShiftLeft"), None);
        assert_eq!(key("Dead", "Quote"), None);
        assert_eq!(key("Unidentified", ""), None);
    }
}
