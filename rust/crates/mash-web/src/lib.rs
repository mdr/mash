//! Mash in the browser.
//!
//! The page (`www/`) shows an xterm.js terminal and runs Mash in a Web Worker, so that a long command doesn't
//! freeze the page. The page sends the worker key presses, pastes and resizes; the worker hands them to a
//! [`WebShell`](js::WebShell) (a [`Shell`](shell::Shell): the REPL with the [`WebPlatform`](platform::WebPlatform))
//! and sends back what to write to the terminal.
//!
//! Ctrl-C while a command runs can't reach the busy worker as a message. Where the page can share memory with
//! the worker (it's served cross-origin isolated: `www/serve.py` does this), the page sets a flag in shared
//! memory, which Mash polls (`Platform::interrupt_requested`). Otherwise the page stops the worker and starts a
//! new session.
//!
//! - [`bridge`]: what Mash needs from the worker's JavaScript
//! - [`platform`]: the browser's `Platform`: files in memory, one user, HTTP via the browser
//! - [`shell`]: the session and its terminal host
//! - [`keys`]: browser keyboard events as the REPL's key presses
//! - [`share`]: links that run a session's commands
//! - `js` (wasm only): the wasm-bindgen interface

pub mod bridge;
pub mod keys;
pub mod platform;
pub mod share;
pub mod shell;

#[cfg(target_arch = "wasm32")]
pub mod js;
