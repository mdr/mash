//! The `mash` command-line shell.
//!
//! ```text
//! mash -e EXPRESSION    evaluate Mash code and print the result
//! mash FILE             run a Mash script
//! mash -c COMMAND       run COMMAND with `sh -c` (as Scala Mash does, for use as a login shell)
//! mash                  the interactive shell (or, if standard input isn't a terminal, run it as a script)
//! ```

mod history_file;
mod startup;
mod terminal;

use std::io::{IsTerminal, Read};
use std::path::PathBuf;
use std::process::ExitCode;
use std::rc::Rc;

use mash_core::eval::{Ctx, RunError, with_big_stack};
use mash_core::platform::Platform;
use mash_core::value::Value;
use mash_os::NativePlatform;
use mash_repl::{Repl, ReplStatus};

const USAGE: &str = "usage: mash -e EXPRESSION | mash FILE | mash -c COMMAND";

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    match args.as_slice() {
        [flag, command, ..] if flag == "-c" => run_via_sh(command),
        [flag, expression] if flag == "-e" => run_mash("<expression>".into(), expression.clone(), true),
        [file] if !file.starts_with('-') => match std::fs::read_to_string(file) {
            Ok(text) => run_mash(file.clone(), text, false),
            Err(e) => {
                eprintln!("mash: {file}: {e}");
                ExitCode::FAILURE
            }
        },
        [] if std::io::stdin().is_terminal() => run_repl(),
        [] => {
            let mut text = String::new();
            match std::io::stdin().read_to_string(&mut text) {
                Ok(_) => run_mash("<stdin>".into(), text, false),
                Err(e) => {
                    eprintln!("mash: could not read standard input: {e}");
                    ExitCode::FAILURE
                }
            }
        }
        _ => {
            eprintln!("{USAGE}");
            ExitCode::from(2)
        }
    }
}

/// Run Mash code on the interpreter's big-stack thread. If `print_result`, print the value (unless it's
/// unit), as Mash's `toString` renders it.
fn run_mash(name: String, text: String, print_result: bool) -> ExitCode {
    with_big_stack(move || {
        let mut ctx = Ctx::new(Rc::new(NativePlatform));
        match ctx.run(&name, &text) {
            Ok(value) => {
                if print_result && !matches!(value, Value::Unit) {
                    match ctx.stringify(&value) {
                        Ok(s) => println!("{s}"),
                        Err(e) => {
                            eprintln!("{}", e.report());
                            return ExitCode::FAILURE;
                        }
                    }
                }
                ExitCode::SUCCESS
            }
            Err(RunError::Syntax(errors)) => {
                for error in errors {
                    let line = text[..usize::from(error.range.start())].matches('\n').count() + 1;
                    eprintln!("syntax error: {} ({name}:{line})", error.message);
                }
                ExitCode::FAILURE
            }
            Err(RunError::Eval(error)) => {
                eprintln!("{}", error.report());
                ExitCode::FAILURE
            }
        }
    })
}

/// The interactive shell: load `~/.mash/lib`, run `~/.mash/init.mash`, then read and run commands until
/// Ctrl-D.
fn run_repl() -> ExitCode {
    with_big_stack(|| {
        let platform = Rc::new(NativePlatform);
        let session_id = uuid(platform.as_ref());
        let mash_dir = platform
            .home_dir()
            .map_or_else(|| PathBuf::from(".mash"), |home| PathBuf::from(home).join(".mash"));
        let mut ctx = Ctx::new(platform);
        // Ctrl-C while a command runs (the terminal is in its normal mode then) interrupts it. While editing,
        // the terminal is raw, and Ctrl-C arrives as a key instead.
        if let Err(e) = signal_hook::flag::register(signal_hook::consts::SIGINT, ctx.interrupt_flag()) {
            eprintln!("mash: could not handle Ctrl-C: {e}");
        }
        startup::load_libraries(&mut ctx, &mash_dir.join("lib"));
        let storage = history_file::FileHistoryStorage::new(mash_dir.join("history"));
        let mut repl = Repl::new(ctx, Box::new(storage), session_id);
        let mut terminal = match terminal::Terminal::new() {
            Ok(terminal) => terminal,
            Err(e) => {
                eprintln!("mash: could not set up the terminal: {e}");
                return ExitCode::FAILURE;
            }
        };
        let init = mash_dir.join("init.mash");
        if let Ok(text) = std::fs::read_to_string(&init) {
            repl.run_script(&init.display().to_string(), &text, &mut terminal);
        }
        repl.start(&mut terminal);
        loop {
            let event = match terminal.read_event() {
                Ok(event) => event,
                Err(e) => {
                    drop(terminal);
                    eprintln!("mash: could not read from the terminal: {e}");
                    return ExitCode::FAILURE;
                }
            };
            if repl.handle_event(event, &mut terminal) == ReplStatus::Exit {
                return ExitCode::SUCCESS;
            }
        }
    })
}

/// A random (version 4) UUID, identifying this session in the history
fn uuid(platform: &dyn Platform) -> String {
    let bits = (u128::from(platform.random_u64()) << 64 | u128::from(platform.random_u64()))
        & !(0xf000 << 64 | 0xc000 << 48)
        | (0x4000 << 64 | 0x8000 << 48);
    let hex = format!("{bits:032x}");
    format!(
        "{}-{}-{}-{}-{}",
        &hex[..8],
        &hex[8..12],
        &hex[12..16],
        &hex[16..20],
        &hex[20..]
    )
}

fn run_via_sh(command: &str) -> ExitCode {
    match std::process::Command::new("sh").arg("-c").arg(command).status() {
        Ok(status) => ExitCode::from(status.code().unwrap_or(1).clamp(0, 255) as u8),
        Err(e) => {
            eprintln!("mash: could not run sh: {e}");
            ExitCode::FAILURE
        }
    }
}

#[cfg(test)]
mod tests {
    use mash_core::platform::TestPlatform;

    #[test]
    fn uuids_are_version_4() {
        let id = super::uuid(&TestPlatform::default());
        assert_eq!(id.len(), 36);
        assert_eq!(&id[14..15], "4");
        assert!("89ab".contains(&id[19..20]), "{id}");
        assert_eq!(id.matches('-').count(), 4);
    }
}
