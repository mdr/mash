//! Running external programs natively.
//!
//! * Standard input is written and standard output read on their own threads while this one waits for the
//!   child, so a child that fills a pipe can never deadlock with us.
//! * When Mash owns the terminal (its standard input is a terminal and it's in the foreground), the child
//!   runs in its own process group and is given the terminal (`tcsetpgrp`): Ctrl-C and Ctrl-Z then go to the
//!   child, not Mash. Afterwards Mash takes the terminal back (with `SIGTTOU` blocked, as it's then in the
//!   background) and restores the terminal settings the child may have changed.
//! * A child killed by Ctrl-C (SIGINT) is reported as interrupted, so evaluation stops.
//! * There's no job control yet: a child stopped by Ctrl-Z is resumed straight away, with a message.
//! * A child with a time limit (from `timeout`) runs in its own process group, which is sent SIGTERM at the
//!   limit, then SIGKILL if it's still running 2 seconds later (as `timeout -k 2` does).

use std::fs::File;
use std::io::{self, IsTerminal, Read, Write};
use std::os::unix::process::CommandExt;
use std::process::{Command, Stdio};
use std::sync::mpsc::{self, RecvTimeoutError};
use std::thread;
use std::time::Duration;

use jiff::Timestamp;
use mash_core::platform::{ProcessInput, ProcessOutcome, ProcessOutput, ProcessRequest};
use nix::errno::Errno;
use nix::sys::signal::{SigSet, SigmaskHow, Signal, kill, killpg};
use nix::sys::termios::{SetArg, Termios, tcgetattr, tcsetattr};
use nix::sys::wait::{WaitPidFlag, WaitStatus, waitpid};
use nix::unistd::{Pid, getpgrp, tcgetpgrp, tcsetpgrp};

/// Mash's terminal, while a child has it
struct Terminal {
    /// Mash's own process group
    group: Pid,
    /// The terminal settings before the child ran
    settings: Option<Termios>,
}

/// Run `f` with `SIGTTOU` blocked on this thread, so that a background process may change the terminal's
/// foreground process group
fn with_sigttou_blocked<T>(f: impl FnOnce() -> T) -> T {
    let mut set = SigSet::empty();
    set.add(Signal::SIGTTOU);
    let previous = set.thread_swap_mask(SigmaskHow::SIG_BLOCK).ok();
    let result = f();
    if let Some(previous) = previous {
        let _ = previous.thread_set_mask();
    }
    result
}

impl Terminal {
    /// The terminal, if Mash's standard input is one and Mash is in the foreground
    fn owned() -> Option<Terminal> {
        let stdin = io::stdin();
        if !stdin.is_terminal() {
            return None;
        }
        let group = getpgrp();
        if tcgetpgrp(&stdin).ok()? != group {
            return None;
        }
        Some(Terminal {
            group,
            settings: tcgetattr(&stdin).ok(),
        })
    }

    /// Make a process group the terminal's foreground group
    fn give_to(&self, group: Pid) {
        with_sigttou_blocked(|| {
            let _ = tcsetpgrp(io::stdin(), group);
        });
    }

    /// Take the terminal back for Mash, with its settings as they were
    fn take_back(&self) {
        with_sigttou_blocked(|| {
            let stdin = io::stdin();
            let _ = tcsetpgrp(&stdin, self.group);
            if let Some(settings) = &self.settings {
                let _ = tcsetattr(&stdin, SetArg::TCSADRAIN, settings);
            }
        });
    }
}

/// Wait for the child to finish, resuming it whenever it stops. Returns its exit status and whether it was
/// killed by SIGINT.
fn wait_for(pid: Pid, own_group: bool, terminal: Option<&Terminal>, program: &str) -> io::Result<(i32, bool)> {
    loop {
        match waitpid(pid, Some(WaitPidFlag::WUNTRACED)) {
            Ok(WaitStatus::Exited(_, code)) => return Ok((code, false)),
            Ok(WaitStatus::Signaled(_, signal, _)) => return Ok((128 + signal as i32, signal == Signal::SIGINT)),
            Ok(WaitStatus::Stopped(_, signal)) => {
                if matches!(signal, Signal::SIGTSTP | Signal::SIGSTOP) {
                    eprintln!("\r\nmash: job control isn't supported yet, so '{program}' was resumed\r");
                }
                // It may have stopped trying to use the terminal before it was given it
                if let Some(terminal) = terminal {
                    terminal.give_to(pid);
                }
                let _ = if own_group {
                    killpg(pid, Signal::SIGCONT)
                } else {
                    kill(pid, Signal::SIGCONT)
                };
            }
            Ok(_) | Err(Errno::EINTR) => {}
            Err(e) => return Err(e.into()),
        }
    }
}

/// How long a timed-out child has to finish after SIGTERM, before SIGKILL
const KILL_AFTER: Duration = Duration::from_secs(2);

/// Signals a child (or its process group) once it has run for its time limit: SIGTERM, then SIGKILL if it's
/// still running [`KILL_AFTER`] later. Stop it when the child has finished.
struct Watchdog {
    finished: mpsc::Sender<()>,
    thread: thread::JoinHandle<()>,
}

impl Watchdog {
    fn start(pid: Pid, own_group: bool, limit: Duration) -> Watchdog {
        let (finished, wait) = mpsc::channel();
        let thread = thread::spawn(move || {
            let send = |signal| {
                let _ = if own_group {
                    killpg(pid, signal)
                } else {
                    kill(pid, signal)
                };
            };
            if wait.recv_timeout(limit) == Err(RecvTimeoutError::Timeout) {
                send(Signal::SIGTERM);
                if wait.recv_timeout(KILL_AFTER) == Err(RecvTimeoutError::Timeout) {
                    send(Signal::SIGKILL);
                }
            }
        });
        Watchdog { finished, thread }
    }

    fn stop(self) {
        let _ = self.finished.send(());
        let _ = self.thread.join();
    }
}

/// Erase any partial last line of the child's output from the terminal, so that the prompt starts afresh
/// (as Scala Mash's `clearPartialOutput`)
fn clear_partial_output() {
    let mut stdout = io::stdout();
    if stdout.is_terminal() {
        let _ = stdout.write_all(b"\r\x1b[K");
        let _ = stdout.flush();
    }
}

pub(crate) fn run_process(request: &ProcessRequest<'_>) -> io::Result<ProcessOutcome> {
    let (program, args) = request
        .argv
        .split_first()
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "no program given"))?;
    let mut command = Command::new(program);
    command
        .args(args)
        .env_clear()
        .envs(request.env.iter().map(|(k, v)| (k, v)))
        .stderr(Stdio::inherit());
    command.stdin(match request.stdin {
        ProcessInput::Inherit => Stdio::inherit(),
        ProcessInput::Text(_) => Stdio::piped(),
        ProcessInput::File(path) => Stdio::from(File::open(path)?),
    });
    command.stdout(match request.stdout {
        ProcessOutput::Inherit => Stdio::inherit(),
        ProcessOutput::Capture => Stdio::piped(),
        ProcessOutput::File(path) => Stdio::from(File::create(path)?),
    });
    let terminal = Terminal::owned();
    // Its own group, so that a time limit stops anything it starts too
    let own_group = terminal.is_some() || request.time_limit.is_some();
    if own_group {
        command.process_group(0);
    }

    let started = Timestamp::now();
    let mut child = command.spawn()?;
    let pid = Pid::from_raw(child.id() as i32);
    if let Some(terminal) = &terminal {
        terminal.give_to(pid);
    }
    let writer = match (request.stdin, child.stdin.take()) {
        (ProcessInput::Text(text), Some(mut stdin)) => {
            let text = text.to_string();
            // A child that exits without reading all its input just closes the pipe early
            Some(thread::spawn(move || {
                let _ = stdin.write_all(text.as_bytes());
            }))
        }
        _ => None,
    };
    let reader = child.stdout.take().map(|mut stdout| {
        thread::spawn(move || {
            let mut bytes = vec![];
            let _ = stdout.read_to_end(&mut bytes);
            bytes
        })
    });

    let watchdog = request.time_limit.map(|limit| Watchdog::start(pid, own_group, limit));
    let waited = wait_for(pid, own_group, terminal.as_ref(), program);
    if let Some(watchdog) = watchdog {
        watchdog.stop();
    }
    if let Some(terminal) = &terminal {
        terminal.take_back();
    }
    let (exit_status, interrupted) = waited?;
    if let Some(writer) = writer {
        let _ = writer.join();
    }
    let stdout = match reader {
        Some(reader) => {
            let bytes = reader.join().unwrap_or_default();
            String::from_utf8(bytes).unwrap_or_else(|e| String::from_utf8_lossy(e.as_bytes()).into_owned())
        }
        None => String::new(),
    };
    let finished = Timestamp::now();
    if request.stdout == ProcessOutput::Inherit {
        clear_partial_output();
    }
    Ok(ProcessOutcome {
        exit_status,
        stdout,
        started,
        finished,
        interrupted,
    })
}
