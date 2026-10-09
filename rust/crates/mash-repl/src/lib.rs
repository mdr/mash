//! Terminal-independent Mash REPL: key events in, a screen model out.
//!
//! This crate must build for `wasm32-unknown-unknown`, so that the same REPL can drive a real terminal
//! (via `mash-cli`) or xterm.js in the browser. It never depends on a terminal library.
//!
//! The host creates a [`Repl`] and feeds it [`InputEvent`]s; the REPL draws itself through the [`Host`] (as
//! ANSI escape sequences) and runs commands between [`Host::begin_command`] and [`Host::end_command`]. See
//! [`repl`] for the loop.
//!
//! - [`line_buffer`], [`state`], [`actions`], [`handlers`], [`editor`], [`history`]: the editor's state and
//!   how actions change it
//! - [`commands`]: running a line and printing its result or error
//! - [`render`]: the state as a [`screen::Screen`]; [`draw`]: a screen as ANSI, drawing only what changed
//!
//! [`InputEvent`]: input::InputEvent
//! [`Host`]: host::Host
//! [`Host::begin_command`]: host::Host::begin_command
//! [`Host::end_command`]: host::Host::end_command

pub mod actions;
pub mod commands;
pub mod config;
pub mod draw;
pub mod editor;
pub mod handlers;
pub mod history;
pub mod host;
pub mod input;
pub mod line_buffer;
pub mod render;
pub mod repl;
pub mod screen;
pub mod state;
pub mod tips;
pub mod view;

pub use repl::{Repl, ReplStatus};
