//! The native terminal, via crossterm: the REPL's [`Host`], and translating crossterm's input events into
//! the REPL's.

use std::io::Write;

use crossterm::event::{
    DisableBracketedPaste, EnableBracketedPaste, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers,
};
use crossterm::{execute, terminal};
use mash_repl::host::Host;
use mash_repl::input::{InputEvent, Key, KeyPress};
use mash_repl::screen::Dimensions;

/// Standard output as a terminal. Raw mode (and bracketed paste) are on while editing, off while a command
/// runs; dropping this restores the terminal.
#[derive(Debug)]
pub(crate) struct Terminal {
    out: std::io::Stdout,
    buffer: String,
    raw: bool,
}

impl Terminal {
    /// Take over the terminal for editing
    pub(crate) fn new() -> std::io::Result<Self> {
        let mut terminal = Terminal {
            out: std::io::stdout(),
            buffer: String::new(),
            raw: false,
        };
        terminal.enter_raw()?;
        Ok(terminal)
    }

    fn enter_raw(&mut self) -> std::io::Result<()> {
        if !self.raw {
            terminal::enable_raw_mode()?;
            execute!(self.out, EnableBracketedPaste)?;
            self.raw = true;
        }
        Ok(())
    }

    fn leave_raw(&mut self) -> std::io::Result<()> {
        if self.raw {
            execute!(self.out, DisableBracketedPaste)?;
            terminal::disable_raw_mode()?;
            self.raw = false;
        }
        Ok(())
    }

    /// Wait for the next input event the REPL cares about
    pub(crate) fn read_event(&mut self) -> std::io::Result<InputEvent> {
        loop {
            if let Some(event) = translate(crossterm::event::read()?) {
                return Ok(event);
            }
        }
    }
}

impl Drop for Terminal {
    fn drop(&mut self) {
        self.flush();
        let _ = self.leave_raw();
    }
}

impl Host for Terminal {
    fn size(&self) -> Dimensions {
        // A size of zero means unknown (e.g. a pseudo-terminal nobody has sized)
        match terminal::size() {
            Ok((columns, rows)) if columns > 0 && rows > 0 => Dimensions::new(rows.into(), columns.into()),
            _ => Dimensions::new(24, 80),
        }
    }

    fn write(&mut self, s: &str) {
        self.buffer.push_str(s);
    }

    fn flush(&mut self) {
        let _ = self.out.write_all(self.buffer.as_bytes());
        let _ = self.out.flush();
        self.buffer.clear();
    }

    fn begin_command(&mut self) {
        self.flush();
        let _ = self.leave_raw();
    }

    fn end_command(&mut self) {
        let _ = self.enter_raw();
    }
}

/// The REPL's version of a crossterm event, if it has one
pub(crate) fn translate(event: Event) -> Option<InputEvent> {
    match event {
        Event::Key(key) => translate_key(key).map(InputEvent::Key),
        Event::Paste(text) => Some(InputEvent::Paste(text)),
        Event::Resize(columns, rows) => Some(InputEvent::Resize(Dimensions::new(rows.into(), columns.into()))),
        Event::FocusGained | Event::FocusLost | Event::Mouse(_) => None,
    }
}

fn translate_key(event: KeyEvent) -> Option<KeyPress> {
    if event.kind == KeyEventKind::Release {
        return None;
    }
    let modifiers = event.modifiers;
    let ctrl = modifiers.contains(KeyModifiers::CONTROL);
    let alt = modifiers.contains(KeyModifiers::ALT);
    let mut shift = modifiers.contains(KeyModifiers::SHIFT);
    let key = match event.code {
        KeyCode::Char(c) => {
            // Shift is already reflected in the character
            shift = false;
            Key::Char(match c {
                // Ctrl with these sends the same control codes as Ctrl-4 to Ctrl-7, which crossterm reports
                '4' if ctrl => '\\',
                '5' if ctrl => ']',
                '6' if ctrl => '^',
                '7' if ctrl => '_',
                c if ctrl => c.to_ascii_lowercase(),
                c => c,
            })
        }
        KeyCode::Enter => Key::Enter,
        KeyCode::Tab => Key::Tab,
        KeyCode::BackTab => {
            shift = true;
            Key::Tab
        }
        KeyCode::Backspace => Key::Backspace,
        KeyCode::Delete => Key::Delete,
        KeyCode::Esc => Key::Escape,
        KeyCode::Up => Key::Up,
        KeyCode::Down => Key::Down,
        KeyCode::Left => Key::Left,
        KeyCode::Right => Key::Right,
        KeyCode::Home => Key::Home,
        KeyCode::End => Key::End,
        KeyCode::PageUp => Key::PageUp,
        KeyCode::PageDown => Key::PageDown,
        KeyCode::F(n) => Key::F(n),
        _ => return None,
    };
    Some(KeyPress { key, ctrl, alt, shift })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn key(code: KeyCode, modifiers: KeyModifiers) -> Option<InputEvent> {
        translate(Event::Key(KeyEvent::new(code, modifiers)))
    }

    #[test]
    fn characters_carry_shift_themselves() {
        assert_eq!(
            key(KeyCode::Char('F'), KeyModifiers::ALT | KeyModifiers::SHIFT),
            Some(InputEvent::Key(KeyPress::alt(Key::Char('F'))))
        );
        assert_eq!(
            key(KeyCode::Char('a'), KeyModifiers::CONTROL),
            Some(InputEvent::Key(KeyPress::ctrl('a')))
        );
    }

    #[test]
    fn control_codes_are_translated() {
        assert_eq!(
            key(KeyCode::Char('7'), KeyModifiers::CONTROL),
            Some(InputEvent::Key(KeyPress::ctrl('_')))
        );
        assert_eq!(
            key(KeyCode::BackTab, KeyModifiers::SHIFT),
            Some(InputEvent::Key(KeyPress::shift(Key::Tab)))
        );
    }

    #[test]
    fn pastes_and_resizes_pass_through() {
        assert_eq!(
            translate(Event::Paste("a\nb".into())),
            Some(InputEvent::Paste("a\nb".into()))
        );
        assert_eq!(
            translate(Event::Resize(100, 30)),
            Some(InputEvent::Resize(Dimensions::new(30, 100)))
        );
    }
}
