//! Test fixtures: an in-memory terminal emulator (Scala `InMemoryTerminal`), a fake host (Scala
//! `DummyTerminal`), and a REPL driven by actions (Scala `AbstractIntegrationTest`).

#![allow(dead_code, unreachable_pub)]

use std::rc::Rc;

use mash_core::eval::Ctx;
use mash_core::platform::{MemoryFiles, TestPlatform};
use mash_core::value::Value;
use mash_repl::actions::Action;
use mash_repl::browser::actions::BrowserAction;
use mash_repl::browser::{BrowserState, SingleObjectState, TwoDTableState};
use mash_repl::history::MemoryHistoryStorage;
use mash_repl::host::Host;
use mash_repl::line_buffer::LineBuffer;
use mash_repl::screen::{BasicColour, Colour, Dimensions, Line, Point, Screen, Style, StyledChar, StyledString};
use mash_repl::state::{CompletionState, IncrementalCompletionState};
use mash_repl::{Repl, ReplStatus};

/// The rows of cells, which rows wrap, and the cursor
type SavedScreen = (Vec<Vec<Option<StyledChar>>>, Vec<bool>, Point);

/// A display-only terminal emulator, enough to check what the screen drawer's output looks like
#[derive(Debug)]
pub struct InMemoryTerminal {
    size: Dimensions,
    /// Each cell holds a character, or None for the second half of a wide character (or a gap)
    rows: Vec<Vec<Option<StyledChar>>>,
    wrapped: Vec<bool>,
    cursor: Point,
    cursor_visible: bool,
    style: Style,
    title: String,
    /// The normal screen (its rows, wrapping and cursor) while the alternate screen is in use
    saved: Option<SavedScreen>,
}

impl InMemoryTerminal {
    pub fn new(size: Dimensions) -> InMemoryTerminal {
        InMemoryTerminal {
            size,
            rows: vec![],
            wrapped: vec![],
            cursor: Point::default(),
            cursor_visible: true,
            style: Style::DEFAULT,
            title: String::new(),
            saved: None,
        }
    }

    /// Interpret `s`, and return what's on the screen
    pub fn send(&mut self, s: &str) -> Screen {
        let mut chars = s.chars().peekable();
        while let Some(c) = chars.next() {
            match c {
                '\x1b' => match chars.next() {
                    Some('[') => {
                        let mut params = String::new();
                        let command = loop {
                            let c = chars.next().expect("unterminated escape sequence");
                            if c.is_ascii_alphabetic() {
                                break c;
                            }
                            params.push(c);
                        };
                        self.csi(&params, command);
                    }
                    Some(']') => {
                        let rest: String = chars.by_ref().take_while(|&c| c != '\x07').collect();
                        self.title = rest.strip_prefix("0;").expect("only titles are supported").to_string();
                    }
                    other => panic!("unknown escape {other:?}"),
                },
                '\r' => self.cursor.column = 0,
                '\n' => self.cursor.row += 1,
                c => self.add_char(c),
            }
        }
        self.screen()
    }

    fn csi(&mut self, params: &str, command: char) {
        let n = || params.parse::<usize>().unwrap_or(1);
        match command {
            'A' => self.cursor.row -= n(),
            'B' => self.cursor.row += n(),
            'C' => self.cursor.column += n(),
            'D' => self.cursor.column -= n(),
            'H' => self.cursor = Point::default(),
            'J' => {
                self.rows.clear();
                self.wrapped.clear();
            }
            'K' => {
                if let Some(row) = self.rows.get_mut(self.cursor.row) {
                    row.truncate(self.cursor.column.min(self.size.columns));
                }
            }
            'l' | 'h' if params == "?25" => self.cursor_visible = command == 'h',
            'h' if params == "?1049" => {
                let rows = std::mem::take(&mut self.rows);
                let wrapped = std::mem::take(&mut self.wrapped);
                self.saved = Some((rows, wrapped, self.cursor));
            }
            'l' if params == "?1049" => {
                let (rows, wrapped, cursor) = self.saved.take().expect("not on the alternate screen");
                (self.rows, self.wrapped, self.cursor) = (rows, wrapped, cursor);
            }
            'm' => self.sgr(params),
            _ => panic!("unknown escape sequence {params:?}{command}"),
        }
    }

    fn sgr(&mut self, params: &str) {
        let codes: Vec<u8> = params.split(';').map(|c| c.parse().unwrap_or(0)).collect();
        let basic = |n: u8| match n {
            0 => BasicColour::Black,
            1 => BasicColour::Red,
            2 => BasicColour::Green,
            3 => BasicColour::Yellow,
            4 => BasicColour::Blue,
            5 => BasicColour::Magenta,
            6 => BasicColour::Cyan,
            _ => BasicColour::Grey,
        };
        let mut i = 0;
        while i < codes.len() {
            let code = codes[i];
            match code {
                0 => self.style = Style::DEFAULT,
                1 => self.style.bold = true,
                4 => self.style.underline = true,
                7 => self.style.inverse = true,
                30..=37 => self.style.foreground = Colour::Basic(basic(code - 30)),
                90..=97 => self.style.foreground = Colour::Bright(basic(code - 90)),
                40..=47 => self.style.background = Colour::Basic(basic(code - 40)),
                100..=107 => self.style.background = Colour::Bright(basic(code - 100)),
                39 => self.style.foreground = Colour::Default,
                49 => self.style.background = Colour::Default,
                38 | 48 => {
                    let colour = Colour::Indexed(codes[i + 2]);
                    if code == 38 {
                        self.style.foreground = colour;
                    } else {
                        self.style.background = colour;
                    }
                    i += 2;
                }
                _ => panic!("unknown SGR code {code}"),
            }
            i += 1;
        }
    }

    fn add_char(&mut self, c: char) {
        let c = StyledChar::new(c, self.style);
        let width = c.width();
        if self.cursor.column + width > self.size.columns {
            self.ensure_row(self.cursor.row);
            self.wrapped[self.cursor.row] = true;
            self.cursor = Point::new(self.cursor.row + 1, 0);
        }
        self.ensure_row(self.cursor.row);
        let row = &mut self.rows[self.cursor.row];
        while row.len() < self.cursor.column + width {
            row.push(Some(StyledChar::new(' ', Style::DEFAULT)));
        }
        row[self.cursor.column] = Some(c);
        for i in 1..width {
            row[self.cursor.column + i] = None;
        }
        self.cursor.column += width;
    }

    fn ensure_row(&mut self, row: usize) {
        while self.rows.len() <= row {
            self.rows.push(vec![]);
            self.wrapped.push(false);
        }
    }

    pub fn screen(&self) -> Screen {
        Screen {
            lines: self
                .rows
                .iter()
                .zip(&self.wrapped)
                .map(|(cells, wrapped)| Line {
                    string: cells.iter().flatten().copied().collect(),
                    ends_in_newline: !wrapped,
                })
                .collect(),
            cursor: self.cursor_visible.then_some(self.cursor),
            title: self.title.clone(),
            alternate: self.saved.is_some(),
        }
    }
}

/// Large enough that nothing wraps or is cut off
pub const LARGE: Dimensions = Dimensions {
    rows: 1000,
    columns: 1000,
};

/// A host that records what's written to it, optionally feeding it to an in-memory terminal
#[derive(Debug)]
pub struct FakeHost {
    pub size: Dimensions,
    pub output: String,
    pub commands_run: usize,
    pub in_command: bool,
}

impl FakeHost {
    pub fn new(size: Dimensions) -> FakeHost {
        FakeHost {
            size,
            output: String::new(),
            commands_run: 0,
            in_command: false,
        }
    }
}

impl Host for FakeHost {
    fn size(&self) -> Dimensions {
        self.size
    }

    fn write(&mut self, s: &str) {
        self.output.push_str(s);
    }

    fn flush(&mut self) {}

    fn begin_command(&mut self) {
        assert!(!self.in_command);
        self.in_command = true;
        self.commands_run += 1;
    }

    fn end_command(&mut self) {
        assert!(self.in_command);
        self.in_command = false;
    }
}

/// A REPL to drive with actions, with test files, home `/home/alice`, and a fixed clock
pub struct TestRepl {
    pub repl: Repl,
    pub host: FakeHost,
    pub platform: Rc<TestPlatform>,
}

impl TestRepl {
    pub fn new() -> TestRepl {
        TestRepl::with(&[], "/home/alice", LARGE)
    }

    pub fn with_size(size: Dimensions) -> TestRepl {
        TestRepl::with(&[], "/home/alice", size)
    }

    pub fn with_files(files: &[&str], cwd: &str) -> TestRepl {
        TestRepl::with(files, cwd, LARGE)
    }

    pub fn with(files: &[&str], cwd: &str, size: Dimensions) -> TestRepl {
        let platform = Rc::new(TestPlatform {
            files: MemoryFiles::of(files),
            cwd: cwd.into(),
            ..TestPlatform::default()
        });
        let ctx = Ctx::new(platform.clone());
        let repl = Repl::new(ctx, Box::new(MemoryHistoryStorage::default()), "test-session");
        TestRepl {
            repl,
            host: FakeHost::new(size),
            platform,
        }
    }

    pub fn act(&mut self, action: Action) -> &mut Self {
        // Each tick of the clock is a second, so history entries are ordered
        let now = self.platform.now.get();
        self.platform.now.set(now + jiff::SignedDuration::from_secs(1));
        let status = self.repl.handle_action(action, &mut self.host);
        assert_eq!(status, ReplStatus::Continue);
        self
    }

    pub fn input(&mut self, s: &str) -> &mut Self {
        self.act(Action::SelfInsert(s.into()))
    }

    pub fn enter(&mut self) -> &mut Self {
        self.act(Action::Enter)
    }

    pub fn complete(&mut self) -> &mut Self {
        self.act(Action::Complete)
    }

    pub fn up(&mut self) -> &mut Self {
        self.act(Action::Up)
    }

    pub fn down(&mut self) -> &mut Self {
        self.act(Action::Down)
    }

    pub fn left(&mut self, n: usize) -> &mut Self {
        for _ in 0..n {
            self.act(Action::BackwardChar);
        }
        self
    }

    pub fn left_extending(&mut self, n: usize) -> &mut Self {
        for _ in 0..n {
            self.act(Action::BackwardCharExtendingSelection);
        }
        self
    }

    pub fn backspace(&mut self) -> &mut Self {
        self.act(Action::BackwardDeleteChar)
    }

    pub fn text(&self) -> &str {
        self.repl.state().line_buffer.text()
    }

    pub fn line_buffer(&self) -> &LineBuffer {
        &self.repl.state().line_buffer
    }

    #[track_caller]
    pub fn assert_buffer(&self, marked: &str) {
        assert_eq!(*self.line_buffer(), LineBuffer::parse_marked(marked));
    }

    /// The value of `it`
    pub fn last_value(&mut self) -> Value {
        self.repl.ctx().globals().get_str("it").expect("no binding for 'it'")
    }

    /// Mash's rendering of `it`
    pub fn last_value_string(&mut self) -> String {
        let value = self.last_value();
        self.repl.ctx().stringify(&value).unwrap()
    }

    pub fn incremental_completion(&self) -> &IncrementalCompletionState {
        match &self.repl.state().completion {
            Some(CompletionState::Incremental(state)) => state,
            other => panic!("not in incremental completion mode: {other:?}"),
        }
    }

    /// Act in the object browser
    pub fn browse_act(&mut self, action: BrowserAction) -> &mut Self {
        self.act(Action::Browser(action))
    }

    /// The object browser's current state
    #[track_caller]
    pub fn browser(&self) -> &BrowserState {
        self.repl
            .state()
            .object_browser
            .as_ref()
            .expect("expected the browser, but no browser is active")
            .current()
    }

    #[track_caller]
    pub fn two_d(&self) -> &TwoDTableState {
        match self.browser() {
            BrowserState::TwoDTable(state) => state,
            other => panic!("not a 2D browser: {other:?}"),
        }
    }

    #[track_caller]
    pub fn single_object(&self) -> &SingleObjectState {
        match self.browser() {
            BrowserState::SingleObject(state) => state,
            other => panic!("not a single object browser: {other:?}"),
        }
    }

    /// The rows of the 2D browser's table, as text
    pub fn two_d_rows(&self) -> Vec<Vec<String>> {
        self.two_d().model.rendered_rows()
    }

    /// The fields of the single object browser's table, as text
    pub fn single_object_rows(&self) -> Vec<(String, String)> {
        self.single_object().model.rendered_fields().to_vec()
    }

    pub fn render(&mut self) -> Screen {
        let size = self.host.size;
        self.repl.render(size)
    }

    /// The screen as text, with `⤦` marking lines that wrap
    pub fn screen_text(&mut self) -> String {
        let screen = self.render();
        screen_text(&screen)
    }
}

pub fn screen_text(screen: &Screen) -> String {
    screen
        .lines
        .iter()
        .map(|line| format!("{}{}", line.string, if line.ends_in_newline { "" } else { "⤦" }))
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn text(s: &StyledString) -> String {
    s.to_string()
}
