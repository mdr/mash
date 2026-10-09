//! A deterministic platform for tests.

use std::cell::{Cell, RefCell};
use std::collections::BTreeMap;
use std::fmt;
use std::io;
use std::sync::Arc;
use std::time::Duration;

use jiff::Timestamp;
use jiff::tz::TimeZone;

use super::{DirEntry, MemoryFiles, Metadata, Platform, ProcessInput, ProcessOutcome, ProcessOutput, ProcessRequest};

/// A deterministic platform for tests: fixed environment, home `/home/alice` (which exists, and is the
/// initial working directory), UTC, a fixed clock (which `sleep` advances instead of sleeping), an in-memory
/// file tree, and fake external commands (`echo`, `cat`, `true`, `false` by default).
#[derive(Debug, Clone)]
pub struct TestPlatform {
    pub env: BTreeMap<String, String>,
    pub home: String,
    /// The initial working directory
    pub cwd: String,
    /// The working directory after [`Platform::set_current_dir`], if it has been called
    pub changed_dir: RefCell<Option<String>>,
    pub time_zone: TimeZone,
    pub files: MemoryFiles,
    pub now: Cell<Timestamp>,
    /// State of the deterministic random number generator
    pub random_state: Cell<u64>,
    /// Everything written with `write_output`, and the output of processes whose output isn't captured
    pub output: RefCell<String>,
    /// External commands, by name (the program's last path segment)
    pub commands: BTreeMap<String, FakeCommand>,
    /// Every process run, in order
    pub processes: RefCell<Vec<ProcessRecord>>,
}

/// A fake external command: computes the outcome of running it.
#[derive(Clone)]
pub struct FakeCommand(pub Arc<dyn Fn(&FakeProcess<'_>) -> FakeOutcome + Send + Sync>);

impl FakeCommand {
    pub fn new(f: impl Fn(&FakeProcess<'_>) -> FakeOutcome + Send + Sync + 'static) -> FakeCommand {
        FakeCommand(Arc::new(f))
    }
}

impl fmt::Debug for FakeCommand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("FakeCommand")
    }
}

/// What a fake command is given.
#[derive(Debug)]
pub struct FakeProcess<'a> {
    /// Arguments, not including the program
    pub args: &'a [String],
    /// Standard input (empty if inherited)
    pub stdin: &'a str,
    pub env: &'a [(String, String)],
    pub cwd: &'a str,
    /// The file system (e.g. for `cat FILE`)
    pub files: &'a MemoryFiles,
}

/// What a fake command does.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FakeOutcome {
    pub exit_status: i32,
    pub stdout: String,
    /// Behave as if killed by Ctrl-C
    pub interrupted: bool,
}

impl FakeOutcome {
    pub fn output(stdout: impl Into<String>) -> FakeOutcome {
        FakeOutcome {
            stdout: stdout.into(),
            ..FakeOutcome::default()
        }
    }

    pub fn status(exit_status: i32) -> FakeOutcome {
        FakeOutcome {
            exit_status,
            ..FakeOutcome::default()
        }
    }
}

/// A process that a [`TestPlatform`] ran.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcessRecord {
    pub argv: Vec<String>,
    pub env: Vec<(String, String)>,
    /// The text given as standard input, if any
    pub stdin: Option<String>,
}

fn default_commands() -> BTreeMap<String, FakeCommand> {
    let mut commands = BTreeMap::new();
    commands.insert(
        "echo".to_string(),
        FakeCommand::new(|p| FakeOutcome::output(format!("{}\n", p.args.join(" ")))),
    );
    commands.insert(
        "cat".to_string(),
        FakeCommand::new(|p| {
            if p.args.is_empty() {
                return FakeOutcome::output(p.stdin);
            }
            let mut out = String::new();
            for file in p.args {
                match p.files.read_file(p.cwd, file) {
                    Ok(bytes) => out.push_str(&String::from_utf8_lossy(&bytes)),
                    Err(_) => {
                        return FakeOutcome {
                            exit_status: 1,
                            stdout: out,
                            interrupted: false,
                        };
                    }
                }
            }
            FakeOutcome::output(out)
        }),
    );
    commands.insert("true".to_string(), FakeCommand::new(|_| FakeOutcome::status(0)));
    commands.insert("false".to_string(), FakeCommand::new(|_| FakeOutcome::status(1)));
    commands
}

impl Default for TestPlatform {
    fn default() -> Self {
        TestPlatform {
            env: BTreeMap::from([("HOME".into(), "/home/alice".into()), ("USER".into(), "alice".into())]),
            home: "/home/alice".into(),
            cwd: "/home/alice".into(),
            changed_dir: RefCell::new(None),
            time_zone: TimeZone::UTC,
            files: MemoryFiles::of(&["/home/alice/"]),
            now: Cell::new("2026-10-09T12:00:00Z".parse().expect("valid timestamp")),
            random_state: Cell::new(0x853c_49e6_748f_ea9b),
            output: RefCell::new(String::new()),
            commands: default_commands(),
            processes: RefCell::new(vec![]),
        }
    }
}

impl TestPlatform {
    /// Add (or replace) a fake external command
    pub fn with_command(
        mut self,
        name: &str,
        f: impl Fn(&FakeProcess<'_>) -> FakeOutcome + Send + Sync + 'static,
    ) -> TestPlatform {
        self.commands.insert(name.to_string(), FakeCommand::new(f));
        self
    }

    fn cwd(&self) -> String {
        self.changed_dir.borrow().clone().unwrap_or_else(|| self.cwd.clone())
    }
}

impl Platform for TestPlatform {
    fn env_var(&self, name: &str) -> Option<String> {
        self.env.get(name).cloned()
    }

    fn env_vars(&self) -> Vec<(String, String)> {
        self.env.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
    }

    fn current_dir(&self) -> Option<String> {
        Some(self.cwd())
    }

    fn home_dir(&self) -> Option<String> {
        Some(self.home.clone())
    }

    fn time_zone(&self) -> TimeZone {
        self.time_zone.clone()
    }

    fn now(&self) -> Timestamp {
        self.now.get()
    }

    fn sleep(&self, duration: Duration) {
        let later = self.now.get().checked_add(duration).unwrap_or(Timestamp::MAX);
        self.now.set(later);
    }

    fn read_dir(&self, path: &str) -> io::Result<Vec<DirEntry>> {
        self.files.read_dir(&self.cwd(), path)
    }

    /// splitmix64
    fn random_u64(&self) -> u64 {
        let state = self.random_state.get().wrapping_add(0x9e37_79b9_7f4a_7c15);
        self.random_state.set(state);
        let mut z = state;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        z ^ (z >> 31)
    }

    fn write_output(&self, text: &str) {
        self.output.borrow_mut().push_str(text);
    }

    fn set_current_dir(&self, path: &str) -> io::Result<()> {
        let real = self.files.canonicalize(&self.cwd(), path)?;
        if !self.files.metadata("/", &real, true)?.is_dir() {
            return Err(io::Error::new(
                io::ErrorKind::NotADirectory,
                format!("Not a directory: {path}"),
            ));
        }
        *self.changed_dir.borrow_mut() = Some(real);
        Ok(())
    }

    fn metadata(&self, path: &str) -> io::Result<Metadata> {
        self.files.metadata(&self.cwd(), path, true)
    }

    fn symlink_metadata(&self, path: &str) -> io::Result<Metadata> {
        self.files.metadata(&self.cwd(), path, false)
    }

    fn read_link(&self, path: &str) -> io::Result<String> {
        self.files.read_link(&self.cwd(), path)
    }

    fn canonicalize(&self, path: &str) -> io::Result<String> {
        self.files.canonicalize(&self.cwd(), path)
    }

    fn read_file(&self, path: &str) -> io::Result<Vec<u8>> {
        self.files.read_file(&self.cwd(), path)
    }

    fn write_file(&self, path: &str, contents: &[u8], append: bool) -> io::Result<()> {
        self.files.write_file(&self.cwd(), path, contents, append, self.now())
    }

    fn create_dir(&self, path: &str, intermediates: bool) -> io::Result<()> {
        self.files.create_dir(&self.cwd(), path, intermediates)
    }

    fn remove(&self, path: &str) -> io::Result<()> {
        self.files.remove(&self.cwd(), path)
    }

    fn copy_file(&self, from: &str, to: &str) -> io::Result<()> {
        self.files.copy_file(&self.cwd(), from, to, self.now())
    }

    fn rename(&self, from: &str, to: &str) -> io::Result<()> {
        self.files.rename(&self.cwd(), from, to)
    }

    fn set_permissions(&self, path: &str, permissions: u32) -> io::Result<()> {
        self.files.set_permissions(&self.cwd(), path, permissions)
    }

    fn create_temp_file(&self) -> io::Result<String> {
        self.files.create_temp(false)
    }

    fn create_temp_dir(&self) -> io::Result<String> {
        self.files.create_temp(true)
    }

    fn run_process(&self, request: &ProcessRequest<'_>) -> io::Result<ProcessOutcome> {
        let (program, args) = request
            .argv
            .split_first()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "no program"))?;
        let name = program.rsplit('/').next().unwrap_or(program);
        let command = self
            .commands
            .get(name)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "No such file or directory"))?
            .clone();
        let cwd = self.cwd();
        let stdin = match request.stdin {
            ProcessInput::Inherit => None,
            ProcessInput::Text(text) => Some(text.to_string()),
            ProcessInput::File(path) => Some(String::from_utf8_lossy(&self.files.read_file(&cwd, path)?).into_owned()),
        };
        self.processes.borrow_mut().push(ProcessRecord {
            argv: request.argv.to_vec(),
            env: request.env.to_vec(),
            stdin: stdin.clone(),
        });
        let started = self.now();
        let outcome = (command.0)(&FakeProcess {
            args,
            stdin: stdin.as_deref().unwrap_or(""),
            env: request.env,
            cwd: &cwd,
            files: &self.files,
        });
        let stdout = match request.stdout {
            ProcessOutput::Inherit => {
                self.output.borrow_mut().push_str(&outcome.stdout);
                String::new()
            }
            ProcessOutput::Capture => outcome.stdout,
            ProcessOutput::File(path) => {
                self.write_file(path, outcome.stdout.as_bytes(), false)?;
                String::new()
            }
        };
        Ok(ProcessOutcome {
            exit_status: outcome.exit_status,
            stdout,
            started,
            finished: self.now(),
            interrupted: outcome.interrupted,
        })
    }
}
