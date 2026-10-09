//! Port of `screen/ScreenDrawerTest.scala`, plus wide characters and erasing. Each drawing is also fed to an
//! in-memory terminal, which must then show the screen drawn.

mod common;

use common::{InMemoryTerminal, LARGE};
use mash_repl::draw::{ScreenDrawer, accept_screen};
use mash_repl::screen::{BasicColour, Dimensions, Line, Point, Screen, Style, StyledString};

const TITLE: &str = "title";

fn screen(lines: Vec<Line>, cursor: Option<Point>) -> Screen {
    Screen {
        lines,
        cursor,
        title: TITLE.into(),
        alternate: false,
    }
}

fn line(s: &str) -> Line {
    Line::new(s.into())
}

fn wrapped(s: &str) -> Line {
    Line {
        string: s.into(),
        ends_in_newline: false,
    }
}

/// The escape sequences made readable, as in the Scala test
fn readable(s: &str) -> String {
    let mut out = String::new();
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '\r' => out += "(carriage-return)",
            '\n' => out += "(newline)",
            '\x1b' => {
                let introducer = chars.next().unwrap();
                if introducer == ']' {
                    let rest: String = chars.by_ref().take_while(|&c| c != '\x07').collect();
                    out += &format!("(set-title {})", rest.trim_start_matches("0;"));
                    continue;
                }
                let mut params = String::new();
                let command = loop {
                    let c = chars.next().unwrap();
                    if c.is_ascii_alphabetic() {
                        break c;
                    }
                    params.push(c);
                };
                out += &match (params.as_str(), command) {
                    (n, 'A') => format!("(cursor-up {n})"),
                    (n, 'C') => format!("(cursor-forward {n})"),
                    (n, 'D') => format!("(cursor-backward {n})"),
                    ("?25", 'l') => "(hide-cursor)".into(),
                    ("?25", 'h') => "(show-cursor)".into(),
                    ("32", 'm') => "(green)".into(),
                    ("33", 'm') => "(yellow)".into(),
                    ("0", 'm') => "(reset)".into(),
                    ("", 'K') => "(erase-line-from-cursor)".into(),
                    (p, c) => format!("(esc {p}{c})"),
                };
            }
            c => out.push(c),
        }
    }
    out
}

fn columns(n: usize) -> Dimensions {
    Dimensions::new(LARGE.rows, n)
}

#[test]
fn empty_screen() {
    let s = screen(vec![], None);
    let draw = ScreenDrawer::new(LARGE.columns).draw(&s, None, None);
    assert_eq!(draw.swapped_out, None);
    assert_eq!(readable(&draw.output), "(hide-cursor)(reset)(set-title title)");
    assert_eq!(InMemoryTerminal::new(LARGE).send(&draw.output), s);
}

#[test]
fn single_line() {
    let s = screen(vec![line("abc")], Some(Point::new(0, 3)));
    let output = ScreenDrawer::new(LARGE.columns).draw(&s, None, None).output;
    assert_eq!(
        readable(&output),
        "(hide-cursor)(reset)abc(set-title title)(show-cursor)"
    );
    assert_eq!(InMemoryTerminal::new(LARGE).send(&output), s);
}

#[test]
fn single_extra_character() {
    let s1 = screen(vec![line("abc")], Some(Point::new(0, 3)));
    let s2 = screen(vec![line("abcd")], Some(Point::new(0, 4)));
    let drawer = ScreenDrawer::new(LARGE.columns);
    let output1 = drawer.draw(&s1, None, None).output;
    let output2 = drawer.draw(&s2, Some(&s1), None).output;
    assert_eq!(
        readable(&output1),
        "(hide-cursor)(reset)abc(set-title title)(show-cursor)"
    );
    assert_eq!(readable(&output2), "(hide-cursor)(reset)d(show-cursor)");
    let mut terminal = InMemoryTerminal::new(LARGE);
    assert_eq!(terminal.send(&output1), s1);
    assert_eq!(terminal.send(&output2), s2);
}

#[test]
fn differences_between_wrapped_lines() {
    let s1 = screen(vec![wrapped("  000"), line("00")], Some(Point::new(1, 2)));
    let s2 = screen(vec![wrapped("   00"), line("000")], Some(Point::new(1, 3)));
    let size = columns(5);
    let drawer = ScreenDrawer::new(5);
    let output1 = drawer.draw(&s1, None, None).output;
    let output2 = drawer.draw(&s2, Some(&s1), None).output;
    assert_eq!(
        readable(&output1),
        "(hide-cursor)(reset)  00000(set-title title)(show-cursor)"
    );
    assert_eq!(
        readable(&output2),
        "(hide-cursor)(reset)(cursor-up 1) (cursor-forward 1)00(cursor-forward 1)0(show-cursor)"
    );
    let mut terminal = InMemoryTerminal::new(size);
    assert_eq!(terminal.send(&output1), s1);
    assert_eq!(terminal.send(&output2), s2);
}

#[test]
fn cursor_in_the_last_column() {
    let s1 = screen(vec![line("123 X")], Some(Point::new(0, 3)));
    let s2 = screen(vec![line("1234X")], Some(Point::new(0, 4)));
    let drawer = ScreenDrawer::new(5);
    let output1 = drawer.draw(&s1, None, None).output;
    let output2 = drawer.draw(&s2, Some(&s1), None).output;
    assert_eq!(
        readable(&output1),
        "(hide-cursor)(reset)123 X(carriage-return)(cursor-forward 3)(set-title title)(show-cursor)"
    );
    assert_eq!(readable(&output2), "(hide-cursor)(reset)4(show-cursor)");
    let mut terminal = InMemoryTerminal::new(columns(5));
    assert_eq!(terminal.send(&output1), s1);
    assert_eq!(terminal.send(&output2), s2);
}

#[test]
fn changing_only_colour_does_not_reset() {
    let mut string = Style::fg(BasicColour::Yellow).apply("Y");
    string.append(&Style::fg(BasicColour::Green).apply("G"));
    let s = screen(vec![Line::new(string)], Some(Point::new(0, 2)));
    let output = ScreenDrawer::new(LARGE.columns).draw(&s, None, None).output;
    assert_eq!(
        readable(&output),
        "(hide-cursor)(reset)(yellow)Y(green)G(set-title title)(show-cursor)"
    );
    assert_eq!(InMemoryTerminal::new(LARGE).send(&output), s);
}

// New cases

#[test]
fn shorter_lines_and_fewer_lines_are_erased() {
    let s1 = screen(vec![line("abcdef"), line("second")], Some(Point::new(1, 6)));
    let s2 = screen(vec![line("abc")], Some(Point::new(0, 3)));
    let drawer = ScreenDrawer::new(LARGE.columns);
    let mut terminal = InMemoryTerminal::new(LARGE);
    terminal.send(&drawer.draw(&s1, None, None).output);
    let after = terminal.send(&drawer.draw(&s2, Some(&s1), None).output);
    assert_eq!(after.lines[0], line("abc"));
    assert_eq!(after.lines[1].string, StyledString::new());
    assert_eq!(after.cursor, Some(Point::new(0, 3)));
}

#[test]
fn wide_characters() {
    // 日本語 is six columns: in five, the third character wraps
    let s1 = screen(vec![wrapped("日本"), line("語x")], Some(Point::new(1, 3)));
    let s2 = screen(vec![wrapped("日本"), line("語yz")], Some(Point::new(1, 4)));
    let drawer = ScreenDrawer::new(5);
    let mut terminal = InMemoryTerminal::new(columns(5));
    assert_eq!(terminal.send(&drawer.draw(&s1, None, None).output), s1);
    let output2 = drawer.draw(&s2, Some(&s1), None).output;
    // From the cursor after `x` back to its column, the third (after the two-column 語)
    assert_eq!(
        readable(&output2),
        "(hide-cursor)(reset)(cursor-backward 1)yz(show-cursor)"
    );
    assert_eq!(terminal.send(&output2), s2);
}

#[test]
fn accepting_moves_past_the_screen() {
    let s = screen(vec![line("one"), line("two"), line("three")], Some(Point::new(0, 3)));
    assert_eq!(
        readable(&accept_screen(&s)),
        "(carriage-return)(newline)(carriage-return)(newline)(carriage-return)(newline)(erase-line-from-cursor)(reset)"
    );
}
