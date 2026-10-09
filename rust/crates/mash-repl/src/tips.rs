//! A tip shown in a box when the REPL starts (Scala `Tips`), unless `config.cli.showStartupTips` is false.

use crate::render::boxes::{BoxContent, render_box};
use crate::render::highlight::render_markup;
use crate::screen::{Dimensions, Line};

/// The tips, with `<mash>…</mash>` marking code
pub const TIPS: &[&str] = &[
    "Ctrl-Space provides context assistance when invoking a function/method.",
    "To use unbound identifiers as strings: <mash>config.language.bareWords = true</mash>",
    "<mash>oldDirs</mash> stores a list of previous working directories from this session.",
    "<mash>up</mash> changes the current directory to the parent.",
    "<mash>glob</mash> can be used to match paths recursively: <mash>glob \"**/*.jpg</mash>",
    "Relative times are available on integers: <mash>1.day.ago</mash>, <mash>3.weeks.fromNow</mash>",
    "Comparisons can be chained: <mash>0 <= i < 10</mash>",
    "Ctrl-r starts an incremental search through history.",
    "Ctrl-q toggles quotation marks around the current region of text.",
    "Alt-. inserts the last argument from history.",
    "Alt-e evaluates the current command (or selection), and replaces the line buffer (or selection) with the result.",
    "Press tab twice to enter completion browsing mode.",
    "Member accesses on lists can be automatically vectorised: <mash>ls.size.sum</mash>",
    "Strings can be interpolated: <mash>\"Hello $user.name\"</mash>",
    "Suppress tips by setting <mash>config.cli.showStartupTips = false</mash>.",
    "Set <mash>config.view.fuzzyTime = true</mash> to view date/times as '3 hours ago', etc.",
    "Set <mash>config.view.browseLargeOutput = false</mash> to avoid starting the browser on long output.",
    "Alt-Up expands the current selection to the nearest enclosing piece of syntax; Alt-Down unexpands.",
    "Shift-left/right/up/down expands the selection.",
    "Ctrl-_ undoes the previous edit",
    "Ctrl-w cuts the current selection, Alt-w copies, and Ctrl-y pastes.",
    "In the object browser, 'e' lets you edit the expression that generates the current data.",
];

/// The box for tip number `index` (modulo the number of tips)
pub fn render_tip(index: usize, size: Dimensions) -> Vec<Line> {
    let content = BoxContent {
        title: "Tip".into(),
        lines: vec![render_markup(TIPS[index % TIPS.len()])],
    };
    render_box(&content, size)
}
