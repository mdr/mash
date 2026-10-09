//! Terminal-independent Mash REPL: key events in, a screen model out.
//!
//! This crate must build for `wasm32-unknown-unknown`, so that the same REPL can drive a real terminal
//! (via `mash-cli`) or xterm.js in the browser. It never depends on a terminal library.
