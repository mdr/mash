//! What the REPL needs from whatever is hosting it: a terminal natively, xterm.js in the browser.

use crate::screen::Dimensions;

pub trait Host {
    /// The terminal's current size
    fn size(&self) -> Dimensions;

    /// Write output: text and ANSI escape sequences. Mash code's own output (`print`, external commands)
    /// goes through the `Platform` instead.
    fn write(&mut self, s: &str);

    fn flush(&mut self);

    /// Called before a command runs: put the terminal into its normal ("cooked") mode, so that commands'
    /// output and external programs behave as usual and Ctrl-C interrupts.
    fn begin_command(&mut self);

    /// Called after a command has run: back to the editor's ("raw") mode.
    fn end_command(&mut self);
}
