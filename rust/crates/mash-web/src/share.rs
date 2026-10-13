//! Sharing a session as a link to the page: `share` builds one from this session's commands, and the page
//! offers to run the commands in a link when it opens (`www/main.js`).
//!
//! The commands go in the URL's fragment, which never reaches the server, as `run` parameters in order:
//! `https://mash-shell.org/try/#run=ls&run=json.read%20%22examples%2Fplanets.json%22`. Each is percent-encoded,
//! so a command can span lines.

use std::cell::RefCell;

use mash_core::eval::{Ctx, R};
use mash_core::function::BuiltinFunction;
use mash_core::infer::Type;
use mash_macros::builtin;

thread_local! {
    /// The page's address and the session's ID, for `share`. One session runs per worker, so this is per
    /// thread rather than per interpreter.
    static SESSION: RefCell<Option<SharedSession>> = const { RefCell::new(None) };
}

#[derive(Debug, Clone)]
struct SharedSession {
    page_url: String,
    session_id: String,
}

/// Record the session that `share` shares: its history entries carry `session_id`, and links lead to
/// `page_url` (any fragment is dropped)
pub fn set_session(page_url: &str, session_id: &str) {
    let page_url = page_url.split('#').next().unwrap_or_default().to_string();
    SESSION.with(|s| {
        *s.borrow_mut() = Some(SharedSession {
            page_url,
            session_id: session_id.to_string(),
        })
    });
}

/// Builtins only in the browser
pub const FUNCTIONS: &[&dyn BuiltinFunction] = &[&ShareFunction];

/// Characters left as they are in a command: unreserved characters, and those allowed in a fragment that
/// don't separate the `run` parameters (`&`, `=`) or mean a space (`+`)
fn is_kept(c: char) -> bool {
    c.is_ascii_alphanumeric() || "-._~!$'()*,;:@/?".contains(c)
}

/// A command as the value of a `run` parameter
pub fn encode_command(command: &str) -> String {
    let mut encoded = String::new();
    for c in command.chars() {
        if is_kept(c) {
            encoded.push(c);
        } else {
            let mut bytes = [0; 4];
            for byte in c.encode_utf8(&mut bytes).bytes() {
                encoded.push_str(&format!("%{byte:02X}"));
            }
        }
    }
    encoded
}

/// A link to the page that offers to run `commands`
pub fn share_url(page_url: &str, commands: &[String]) -> String {
    let runs: Vec<String> = commands.iter().map(|c| format!("run={}", encode_command(c))).collect();
    format!("{page_url}#{}", runs.join("&"))
}

/// Make a link that runs this session's commands when opened
///
/// The link is also copied to the clipboard. Whoever opens it is asked before the commands run.
///
/// <mash>
///    share      # all the commands so far in this session
///    share 3    # the last three
/// </mash>
#[builtin(namespace = "web", returns = Type::STRING)]
pub fn share(ctx: &mut Ctx, #[param("Number of commands, counting back from the last")] n: Option<usize>) -> R<String> {
    let Some(session) = SESSION.with(|s| s.borrow().clone()) else {
        return Err(mash_core::eval::MashError::new("There's no session to share"));
    };
    let entries = ctx.command_history().map(|h| h.entries()).unwrap_or_default();
    let mut commands: Vec<String> = entries
        .into_iter()
        .filter(|e| e.session_id == session.session_id)
        // A command run in mish mode is mish with a `!` in front, wherever it's run
        .map(|e| {
            if e.mish && !e.command.starts_with('!') {
                format!("!{}", e.command)
            } else {
                e.command
            }
        })
        .collect();
    if let Some(n) = n {
        commands.drain(..commands.len().saturating_sub(n));
    }
    let url = share_url(&session.page_url, &commands);
    // Copying is a convenience: the link is the result either way
    let _ = ctx.platform().set_clipboard(&url);
    Ok(url)
}
