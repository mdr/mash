//! The object browser's actions and key maps (Scala `ObjectBrowserActions` and `ObjectBrowserKeyMap`, with
//! its `IncrementalSearch` and `ExpressionInput` maps).

use crate::actions::Action;
use crate::input::{Key, KeyPress};

/// Something the user asked the object browser to do.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BrowserAction {
    NextItem,
    PreviousItem,
    NextPage,
    PreviousPage,
    FirstItem,
    LastItem,
    NextColumn,
    PreviousColumn,
    FirstColumn,
    LastColumn,
    /// Select whole rows again, rather than a column
    UnfocusColumn,
    /// Show the next item (or field) of the value this one came from, in its place
    NextParentItem,
    PreviousParentItem,
    /// Browse the selected part of the value
    Focus,
    /// Back to the previous value
    Back,
    /// Leave the browser
    Exit,
    /// Insert the expression for the selection into the command line, and leave the browser
    InsertItem,
    /// Insert the expression for the whole value into the command line, and leave the browser
    InsertWholeItem,
    ToggleMarked,
    /// Rebuild the view (after the terminal changes size) and redraw it
    Rerender,
    /// Toggle between a tree and the usual view
    ViewAsTree,
    /// View as one-dimensional: an object's fields, or a list's items as lines
    View1D,
    /// View as a 2D table
    View2D,
    HideColumn,
    /// Open the selection (`open`)
    Open,
    /// Browse the children of the selected directory
    FocusDirectory,
    /// Browse the lines of the selected file
    ReadFile,
    /// Copy the selection to the clipboard
    Copy,
    /// Begin an incremental search
    BeginSearch,
    /// Delete the last character of the search
    Unsearch,
    NextHit,
    PreviousHit,
    ExitSearch,
    /// Toggle case-sensitive searching
    ToggleCase,
    /// Begin typing an expression to evaluate against the value
    BeginExpression,
}

/// Printable characters insert themselves (and do nothing unless searching); other unbound keys do nothing
fn unbound(key: KeyPress) -> Action {
    match key {
        KeyPress {
            key: Key::Char(c),
            ctrl: false,
            alt: false,
            ..
        } if !c.is_control() => Action::SelfInsert(c.to_string()),
        _ => Action::Noop,
    }
}

/// Scala `ObjectBrowserKeyMap`
pub(crate) fn browser_action(key: KeyPress) -> Action {
    use BrowserAction::*;
    let KeyPress { key: k, ctrl, alt, .. } = key;
    let action = match (k, ctrl, alt) {
        (Key::Char('l'), true, false) => return Action::RedrawScreen,
        (Key::Right, false, false) | (Key::Char('f'), true, false) => NextColumn,
        (Key::Left, false, false) | (Key::Char('b'), true, false) => PreviousColumn,
        (Key::Char('a'), true, false) => FirstColumn,
        (Key::Char('e'), true, false) => LastColumn,
        (Key::Up, false, false) | (Key::Char('p'), true, false) => PreviousItem,
        (Key::Down, false, false) | (Key::Char('n'), true, false) => NextItem,
        (Key::PageUp, false, false) => PreviousPage,
        (Key::PageDown, false, false) | (Key::Char(' '), false, false) => NextPage,
        (Key::Char('q'), false, false) | (Key::Char('g'), true, false) => Exit,
        (Key::Char('g'), false, false) | (Key::Home, false, false) | (Key::Char('<'), false, true) => FirstItem,
        (Key::Char('G'), false, false) | (Key::End, false, false) | (Key::Char('>'), false, true) => LastItem,
        (Key::Char('N'), false, false) => NextParentItem,
        (Key::Char('P'), false, false) => PreviousParentItem,
        (Key::Char('R'), false, false) => UnfocusColumn,
        (Key::Char('f'), false, false) | (Key::Enter, false, false) => Focus,
        (Key::Char('d'), false, false) => FocusDirectory,
        (Key::Char('r'), false, false) => ReadFile,
        (Key::Char('b'), false, false) => Back,
        (Key::Char('m'), false, false) => ToggleMarked,
        (Key::Char('i'), false, false) => InsertItem,
        (Key::Char('I'), false, false) => InsertWholeItem,
        (Key::Char('t'), false, false) => ViewAsTree,
        (Key::Char('1'), false, false) => View1D,
        (Key::Char('2'), false, false) => View2D,
        (Key::Char('h'), false, false) => HideColumn,
        (Key::Char('o'), false, false) => Open,
        (Key::Char('c'), false, false) => Copy,
        (Key::Char('/'), false, false) => BeginSearch,
        (Key::Char('e'), false, false) => BeginExpression,
        _ => return unbound(key),
    };
    Action::Browser(action)
}

/// Scala `ObjectBrowserKeyMap.IncrementalSearch`: printable characters extend the search
pub(crate) fn search_action(key: KeyPress) -> Action {
    use BrowserAction::*;
    let action = match (key.key, key.ctrl, key.alt) {
        (Key::Char('l'), true, false) => return Action::RedrawScreen,
        (Key::Enter, false, false) | (Key::Char('g'), true, false) => ExitSearch,
        (Key::Backspace, false, false) => Unsearch,
        (Key::Char('t'), true, false) => ToggleCase,
        (Key::Up, false, false) | (Key::Char('p'), true, false) => PreviousHit,
        (Key::Down, false, false) | (Key::Char('n'), true, false) => NextHit,
        _ => return unbound(key),
    };
    Action::Browser(action)
}

/// Scala `ObjectBrowserKeyMap.ExpressionInput`: line editing (but not history, undo or the other modes),
/// given the normal key map's action for the key
pub(crate) fn expression_action(normal: Action) -> Action {
    use Action::*;
    match normal {
        Up
        | Down
        | BackwardComplete
        | IncrementalHistorySearch
        | PageUp
        | PageDown
        | InsertLastArg
        | ToggleMish
        | BrowseLastResult
        | Undo
        | Redo
        | EndOfFile => Noop,
        action => action,
    }
}
