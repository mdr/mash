//! The `mash` command-line shell.
//!
//! ```text
//! mash -e EXPRESSION    evaluate Mash code and print the result
//! mash FILE             run a Mash script
//! mash -c COMMAND       run COMMAND with `sh -c` (as Scala Mash does, for use as a login shell)
//! ```
//!
//! The interactive shell arrives in a later milestone.

use std::process::ExitCode;
use std::rc::Rc;

use mash_core::eval::{Ctx, RunError, with_big_stack};
use mash_core::value::Value;
use mash_os::NativePlatform;

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
        [] => {
            eprintln!(
                "mash {}: the interactive shell is not available yet\n{USAGE}",
                env!("CARGO_PKG_VERSION")
            );
            ExitCode::FAILURE
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

fn run_via_sh(command: &str) -> ExitCode {
    match std::process::Command::new("sh").arg("-c").arg(command).status() {
        Ok(status) => ExitCode::from(status.code().unwrap_or(1).clamp(0, 255) as u8),
        Err(e) => {
            eprintln!("mash: could not run sh: {e}");
            ExitCode::FAILURE
        }
    }
}
