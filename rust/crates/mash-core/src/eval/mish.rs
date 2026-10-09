//! Running system commands: `!{…}` / `!!{…}` and mish lines (Scala `MishEvaluator`), and `!cmd` functions
//! (Scala `SystemCommandFunction`).
//!
//! `!{cmd args}` captures standard output and gives an `os.ProcessResult`; `!!{cmd args}` and mish lines
//! run with the terminal (standard input and output inherited) and give unit. `cd` is handled by Mash
//! itself.

use std::sync::LazyLock;

use crate::ast::{E, MishRedirect, RedirectOp};
use crate::builtins::os::mish_cd;
use crate::builtins::os::process::{Input, Output, process_result, run_command, stdin_text};
use crate::function::{Param, Params};
use crate::value::{Scope, Value};

use super::{Ctx, Location, MashError, R};

/// Parameters of a `!{…}` expression called as a function: `!{cat} "text"`
pub(crate) static MISH_PARAMS: LazyLock<Params> = LazyLock::new(|| {
    Params::new(vec![
        Param::named("standardIn").summary("What to send to standard input"),
    ])
});

/// Parameters of a system command function (`!ls`)
pub(crate) static SYSTEM_COMMAND_PARAMS: LazyLock<Params> =
    LazyLock::new(|| Params::new(vec![Param::named("args").summary("Arguments").variadic()]));

/// Give an error without a location this one
fn located(error: MashError, location: Option<Location>) -> MashError {
    if error.location().is_none() && !error.is_interrupted() {
        MashError::at(error.message, location)
    } else {
        error
    }
}

impl Ctx {
    /// Evaluate a mish expression, with `stdin` (if given) as text for the command's standard input
    #[allow(clippy::too_many_arguments)]
    #[inline(never)]
    pub(crate) fn mish(
        &mut self,
        e: &E,
        command: &E,
        args: &[E],
        redirects: &[MishRedirect],
        capture_output: bool,
        stdin: Option<Value>,
        scope: &Scope,
    ) -> R<Value> {
        let command_value = self.eval(command, scope)?;
        let mut arg_values = Vec::with_capacity(args.len());
        for arg in args {
            arg_values.push(self.eval(arg, scope)?);
        }
        let redirect = |ctx: &mut Ctx, op: RedirectOp| -> R<Option<String>> {
            match redirects.iter().find(|r| r.op == op) {
                Some(r) => {
                    let value = ctx.eval(&r.arg, scope)?;
                    Ok(Some(ctx.stringify(&value)?))
                }
                None => Ok(None),
            }
        };
        let stdin_file = redirect(self, RedirectOp::StandardInput)?;
        let stdout_file = redirect(self, RedirectOp::StandardOutput)?;
        let stdin_text = match &stdin {
            Some(value) => Some(stdin_text(self, value)?),
            None => None,
        };

        if command_value.as_str() == Some("cd") {
            let location = match arg_values.len() {
                0 => self.loc(e.range),
                1 => self.loc(args[0].range),
                _ => self.loc(args[1].range),
            };
            mish_cd(self, &arg_values).map_err(|error| located(error, location))?;
            return Ok(Value::Unit);
        }

        let mut words = vec![command_value];
        for value in arg_values {
            match value {
                Value::List(xs) => words.extend(xs.snapshot()),
                x => words.push(x),
            }
        }
        let input = match (stdin_file, stdin_text) {
            (Some(file), _) => Input::File(file),
            (None, Some(text)) => Input::Text(text),
            (None, None) => Input::Inherit,
        };
        let output = match stdout_file {
            Some(file) => Output::File(file),
            None if capture_output => Output::Capture,
            None => Output::Inherit,
        };
        let outcome = run_command(self, &words, input, output).map_err(|error| located(error, self.loc(e.range)))?;
        Ok(if capture_output {
            process_result(outcome)
        } else {
            Value::Unit
        })
    }

    /// Call a system command function (`!ls`): run the command with the arguments, using the terminal
    pub(crate) fn call_system_command(&mut self, command: &str, args: Value) -> R<Value> {
        let mut words = vec![Value::str(command)];
        match args {
            Value::List(xs) => match xs.snapshot().as_slice() {
                [Value::List(inner)] => words.extend(inner.snapshot()),
                items => words.extend(items.iter().cloned()),
            },
            x => words.push(x),
        }
        run_command(self, &words, Input::Inherit, Output::Inherit)?;
        Ok(Value::Unit)
    }
}
